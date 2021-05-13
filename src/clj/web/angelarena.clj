(ns web.angelarena
  (:require [clojure.string :refer [lower-case capitalize]]
            [game.utils :refer [in-coll?]]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]
            [web.lobby :refer [client-gameids refresh-lobby]]
            [web.mongodb :refer [object-id]]
            [web.utils :refer [response json-response average]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clj-time.core :as t]))

(defonce arena-supported-formats [:standard :startup])
(defonce arena-queue (atom []))
(defonce arena-queue-times (atom (into (hash-map)
                                       (map (fn [form] [form {:corp [] :runner []}])
                                            arena-supported-formats))))

(defn- get-runs
  [db username]
  (try
    (let [{:keys [angelarena-runs]}
          (mc/find-one-as-map db "users" {:username username} ["angelarena-runs"])]
      (merge (into (hash-map)
                   (map (fn [form] [form {:corp nil :runner nil}])
                        arena-supported-formats))
             angelarena-runs))))

(defn- get-deck-from-id
  [db username deck-id]
  (try
    (let [map-card (fn [c] (update-in c [:card] @all-cards))
          unknown-card (fn [c] (nil? (:card c)))]
      (as-> (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}) d
        (update-in d [:cards] #(mapv map-card %))
        (update-in d [:cards] #(vec (remove unknown-card %)))
        (update-in d [:identity] #(@all-cards (:title %)))
        (assoc d :status (calculate-deck-status d))))))

(defn- get-current-deck
  [db username form side]
  (let [runs (get-runs db username)
        deck-id (get-in runs [form side :deck-id])]
    (get-deck-from-id db username deck-id)))

(defn fetch-runs
  [{db :system/db
    {username :username} :user}]
  (if username
    (json-response 200 (get-runs db username))
    (response 401 {:message "Unauthorized"})))

(defmethod ws/-msg-handler :angelarena/start-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    client-id :client-id
    {:keys [deck-id]} :?data}]
  (when username
    (try
      (let [runs (get-runs db username)
            deck (get-deck-from-id db username deck-id)
            form (keyword (lower-case (get-in deck [:status :format])))
            side (keyword (lower-case (get-in deck [:identity :side])))]
        (when-not (get-in runs [form side]) ; already running on this side and format
          (when (get-in deck [:status form :legal]) ; deck is legal in this format
            (mc/update db "users"
                       {:username username}
                       {"$set" {:angelarena-runs
                                (assoc-in runs [form side]
                                          {:deck-id deck-id
                                           :wins 0
                                           :losses 0
                                           :run-started (java.util.Date.)})}})
            (mc/update db "decks"
                       {:_id (object-id deck-id) :username username}
                       {"$set" {:locked true}})))))))

(defmethod ws/-msg-handler :angelarena/abandon-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    client-id :client-id
    {:keys [deck-id]} :?data}]
  (when username
    (try
      (let [runs (get-runs db username)
            deck (get-deck-from-id db username deck-id)
            form (keyword (lower-case (get-in deck [:status :format])))
            side (keyword (lower-case (get-in deck [:identity :side])))]
        (when (get-in runs [form side]) ; there's a run in this side and format
          (mc/update db "users"
                     {:username username}
                     {"$set" {:angelarena-runs
                              (assoc-in runs [form side] nil)}})
          (mc/update db "decks"
                     {:_id (object-id deck-id) :username username}
                     {"$set" {:locked false}}))))))

(defn- remove-from-queue [username]
  (swap! arena-queue (partial remove #(= username (get-in % [:user :username])))))

(defn- add-queue-time [player]
  (let [side (keyword (lower-case (:side player)))
        form (:format player)
        queue-time (t/in-seconds (t/interval (:queue-start player) (t/now)))]
    ; keep the latest 5 wait times
    (if (> (count (get-in @arena-queue-times [form side])) 5)
      (swap! arena-queue-times update-in [form side] #(conj (drop 1 %) queue-time))
      (swap! arena-queue-times update-in [form side] conj queue-time))))

(defn fetch-queue-times
  [{db :system/db
    {username :username} :user}]
  (if username
    (json-response 200
                   (into (hash-map)
                         (map (fn [form] [form {:corp (average (get-in @arena-queue-times [form :corp] []))
                                                :runner (average (get-in @arena-queue-times [form :runner] []))}])
                              arena-supported-formats)))
    (response 401 {:message "Unauthorized"})))

(defn- start-game
  [event player1 player2 form]
  (let [gameid (java.util.UUID/randomUUID)
        game {:date            (java.util.Date.)
              :gameid          gameid
              :title           (str "Match between "
                                    (get-in player1 [:user :username])
                                    " and "
                                    (get-in player2 [:user :username]))
              :allow-spectator true
              :save-replay     true
              :api-access      true
              :spectatorhands  false
              :mute-spectators true
              :password        nil
              :room            "angelarena"
              :format          (name form)
              :players         [player1 player2]
              :spectators      []
              :spectator-count 0
              :timer           nil
              :messages        [{:user "__system__"
                                 :text "Angel Arena lobby has been created."}
                                {:user "__system__"
                                 :text "Here will be some explanations about the rules of Angel Arena."} ]
              :last-update     (t/now)}]
    (refresh-lobby gameid game)
    (swap! client-gameids assoc (:ws-id player1) gameid (:ws-id player2) gameid)
    ; send clients message to make them join lobby
    (ws/broadcast-to! [(:ws-id player1) (:ws-id player2)] :lobby/select {:gameid gameid})
    ; send server message to start game
    (ws/event-msg-handler (assoc event :id :netrunner/start))))

(defmethod ws/-msg-handler :angelarena/queue
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id
    {:keys [deck-id]} :?data
    :as event}]
  (when username
    (let [runs (get-runs db username)
          deck (get-deck-from-id db username deck-id)
          form (keyword (lower-case (get-in deck [:status :format])))
          side (keyword (lower-case (get-in deck [:identity :side])))
          run-info (get-in runs [form side])]
      (when (and runs deck form side
                 ; check that player isn't already queueing
                 (empty? (filter #(= username (:username %)) @arena-queue)))
        (let [player {:user user
                      :ws-id client-id
                      :format form
                      :side (capitalize (name side))
                      :deck deck
                      :run-info run-info
                      :queue-start (t/now)}
              other-side (if (= :corp side) "Runner" "Corp")
              eligible-players (->> @arena-queue
                                    ; Players in the same format playing the other side
                                    (filter #(and (= form (:format %))
                                                  (= other-side (:side %))))
                                    ;XXX: Remove players you already played against from eligible pool
                                    ; Players that didn't block us
                                    (remove #(in-coll?
                                               (get-in % [:user :options :blocked-users])
                                               username))
                                    ; Players that we didn't block
                                    (remove #(in-coll?
                                               (get-in player [:user :options :blocked-users])
                                               (get-in % [:user :username]))))
              match (first eligible-players)]
          (if match
            (do
              (remove-from-queue (get-in match [:user :username]))
              (add-queue-time player)
              (add-queue-time match)
              (start-game event (dissoc player :queue-start) (dissoc match :queue-start) form))
            (swap! arena-queue conj player)))))))

(defmethod ws/-msg-handler :angelarena/dequeue
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id}]
  (when username
    (remove-from-queue username)))
