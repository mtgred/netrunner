(ns web.angel-arena.utils
  (:require
   [jinteki.cards :refer [all-cards]]
   [jinteki.validator :refer [calculate-deck-status]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [web.mongodb :refer [->object-id]]
   [web.ws :as ws]))

(def supported-formats [:standard :startup :eternal])

(defmethod ws/-msg-handler :angel-arena/formats
  angel-arena--formats
  [{uid :uid}]
  (when uid
    (ws/chsk-send! uid [:angel-arena/formats supported-formats])))

; After this time without any activity, a warning is issued.
(def inactive-period-warning 30)

; This period starts counting when the first period runs out. After this period,
; the game can be cancelled or claimed as a victory.
; This period is also used for the initial period on turn 0.
(def inactive-period-countdown 5)

; How many times a player is allowed to ask for more time.
(def max-inactivity-count 3)

(defn get-runs
  [db username]
  (try
    (let [{:keys [angel-arena-runs]}
          (mc/find-one-as-map db "users" {:username username} ["angel-arena-runs"])]
      angel-arena-runs)
    (catch Exception e
      (println "Caught exception searching for run: " (.getMessage e)))))

(defn get-deck-from-id
  [db username deck-id]
  (try
    (let [map-card (fn [c] (update c :card @all-cards))
          unknown-card (fn [c] (nil? (:card c)))]
      (as-> (mc/find-one-as-map db "decks" {:_id (->object-id deck-id) :username username}) d
        (update d :cards #(mapv map-card %))
        (update d :cards #(vec (remove unknown-card %)))
        (update d :identity #(@all-cards (:title %)))
        (assoc d :status (calculate-deck-status d))))
    (catch Exception e
      (println "Caught exception searching for a deck from deck-id: " (.getMessage e)))))

(defn get-current-deck
  [db username form side]
  (let [runs (get-runs db username)
        deck-id (get-in runs [form side :deck-id])]
    (get-deck-from-id db username deck-id)))

(defn get-wins
  [{:keys [games side]}]
  (count (filter #(= (name side) (:winner %)) games)))

(defn get-losses
  [{:keys [games] :as run-info}]
  (- (count (remove #(nil? (:winner %)) games))
     (get-wins run-info)))
