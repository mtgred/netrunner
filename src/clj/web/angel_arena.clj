(ns web.angel-arena
  (:require [clojure.string :refer [lower-case capitalize]]
            [game.core.say :refer [system-msg]]
            [game.core.winning :refer [win]]
            [game.utils :refer [in-coll?]]
            [jinteki.utils :refer [other-side]]
            [web.angel-arena.runs :refer [start-run finish-run add-new-match]]
            [web.angel-arena.utils :refer [supported-formats get-runs get-deck-from-id get-current-deck]]
            [web.game :refer [swap-and-send-diffs!]]
            [web.lobby :refer [all-games client-gameids game-for-id close-lobby refresh-lobby refresh-lobby-assoc-in]]
            [web.utils :refer [response json-response average]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clj-time.core :as t]))

(defonce arena-queue (atom []))
(defonce arena-queue-times (atom (into (hash-map)
                                       (map (fn [form] [form {:corp [] :runner []}])
                                            supported-formats))))
(defonce inactivity-periods [5 5])
(defonce max-inactivity-count 3)

(defn fetch-runs
  [{db :system/db
    {username :username} :user}]
  (if username
    (json-response 200 (get-runs db username))
    (response 401 {:message "Unauthorized"})))

(defmethod ws/-msg-handler :angel-arena/start-run
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
        (when-not (get-in runs [form side]) ; when not already running on this side and format
          (start-run db username runs deck)))
      (catch Exception e
        (println "Caught exception while starting a new run: " (.getMessage e))))))

(defmethod ws/-msg-handler :angel-arena/abandon-run
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
          (finish-run db username runs deck)))
      (catch Exception e
        (println "Caught exception while abandoning run: " (.getMessage e))))))

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
                              supported-formats)))
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
              :room            "angel-arena"
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
    (ws/event-msg-handler (assoc event :id :netrunner/start))
    gameid))

(defmethod ws/-msg-handler :angel-arena/queue
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
              (when-let [gameid (str (start-game event (dissoc player :queue-start) (dissoc match :queue-start) form))]
                (add-new-match db player match gameid)
                (add-new-match db match player gameid)))
            (swap! arena-queue conj player)))))))

(defmethod ws/-msg-handler :angel-arena/dequeue
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id}]
  (when username
    (remove-from-queue username)))

(defn check-for-inactivity
  "Called by a background thread to notify lobbies without activity."
  [db]
  (doseq [{:keys [state gameid last-update-only-actions started] :as game} (filter #(= "angel-arena" (:room %)) (vals @all-games))]
    (let [inactive-side (if (:end-turn @state)
                          (other-side (:active-player @state))
                          (:active-player @state))
          inactive-user (get-in @state [inactive-side :user])]
      (when-not (or (nil? gameid)
                    (zero? (:turn @state))
                    (:run @state))
        (case (get-in @state [:angel-arena-info :inactivity-warning :stage] 0)
          0 (when (t/after? (t/now) (t/plus last-update-only-actions (t/seconds (first inactivity-periods))))
              ; no action for longer than first inactivity-period
              (swap! state assoc-in [:angel-arena-info :inactivity-warning]
                     {:stage 1
                      :inactive-side inactive-side
                      :inactive-user inactive-user
                      :warning-time (t/now)
                      :period-to-react (second inactivity-periods)})
              (swap-and-send-diffs! game))
          1 (when-let [{:keys [warning-time period-to-react]} (get-in @state [:angel-arena-info :inactivity-warning])]
              (if (t/after? last-update-only-actions warning-time)
                ; there was an action after the warning
                (do (swap! state update-in [:angel-arena-info] dissoc :inactivity-warning)
                    (swap-and-send-diffs! game))
                ; still no action
                (when (t/after? (t/now) (t/plus warning-time (t/seconds period-to-react)))
                  ; reaction period over
                  (do (swap! state assoc-in [:angel-arena-info :inactivity-warning :stage] 2)
                      (swap-and-send-diffs! game)))))
          (when-let [{:keys [warning-time]} (get-in @state [:angel-arena-info :inactivity-warning])]
            (when (t/after? last-update-only-actions warning-time)
              ; there was an action after the warning
              (swap! state update-in [:angel-arena-info] dissoc :inactivity-warning)
              (swap-and-send-diffs! game))))))))

(defmethod ws/-msg-handler :angel-arena/more-time
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id
    {:keys [gameid]} :?data}]
  (when-let [{:keys [state] :as game} (game-for-id (java.util.UUID/fromString gameid))]
    (when-let [{:keys [stage inactive-side inactive-user warning-time period-to-react]}
               (get-in @state [:angel-arena-info :inactivity-warning])]
      (when (and (= username (:username inactive-user))
                 (pos? (get-in @state [:angel-arena-info :inactivity-counter inactive-side] 1)))
        (swap! state update-in [:angel-arena-info :inactivity-counter inactive-side] (fnil dec max-inactivity-count))
        (system-msg state inactive-side (str "has asked for more time ("
                                             (get-in @state [:angel-arena-info :inactivity-counter inactive-side])
                                             " remaining)"))
        (swap! state update :angel-arena-info dissoc :inactivity-warning)
        (swap-and-send-diffs! game)
        (refresh-lobby-assoc-in (java.util.UUID/fromString gameid) [:last-update] (t/now))
        (refresh-lobby-assoc-in (java.util.UUID/fromString gameid) [:last-update-only-actions] (t/now))))))

(defmethod ws/-msg-handler :angel-arena/claim-victory
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id
    {:keys [gameid]} :?data}]
  (when-let [{:keys [state] :as game} (game-for-id (java.util.UUID/fromString gameid))]
    (when-let [{:keys [stage inactive-side inactive-user warning-time period-to-react]}
               (get-in @state [:angel-arena-info :inactivity-warning])]
      (when (and (= 2 stage)
                 (= username (get-in @state [(other-side inactive-side) :user :username])))
        (system-msg state (other-side inactive-side) "claims a victory")
        (win state (other-side inactive-side) "Claim")
        (swap-and-send-diffs! game)
        (close-lobby db game)))))

(defmethod ws/-msg-handler :angel-arena/cancel-match
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id
    {:keys [gameid]} :?data}]
  (when-let [{:keys [state] :as game} (game-for-id (java.util.UUID/fromString gameid))]
    (when-let [{:keys [stage inactive-side inactive-user warning-time period-to-react]}
               (get-in @state [:angel-arena-info :inactivity-warning])]
      (when (and (= 2 stage)
                 (= username (get-in @state [(other-side inactive-side) :user :username])))
        (system-msg state (other-side inactive-side) "cancels the match")
        (swap-and-send-diffs! game)
        (close-lobby db game)))))
