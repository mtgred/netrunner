(ns web.diffs)

(defn user-public-view
  "Strips private server information from a player map."
  [{:keys [started format]} player]
  (as-> player p
        (dissoc p :ws-id)
        (if-let [{:keys [_id] :as deck} (:deck p)]
          (let [legal (get-in deck [:status (keyword format) :legal])
                status {:format format
                        (keyword format) {:legal legal}}
                deck (-> deck
                         (select-keys
                           (if started
                             [:name :date :identity]
                             [:name :date]))
                         (assoc :_id (str _id) :status status))]
            (assoc p :deck deck))
          p)))

(defn update-if-contains
  [m ks f & args]
  (if (get m ks)
    (apply (partial update m ks f) args)
    m))

(defn game-internal-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [full-game game-update]
    (-> game-update
        (dissoc :state :last-update :on-close)
        (update-if-contains :players #(map (partial user-public-view full-game) %))
        (update-if-contains :original-players #(map (partial user-public-view full-game) %))
        (update-if-contains :spectators #(map (partial user-public-view full-game) %))
        (update-if-contains :ending-players #(map (partial user-public-view full-game) %))))
