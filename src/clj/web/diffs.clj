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

(defn game-public-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [game]
  (-> game
      (dissoc :state :last-update :on-close)
      (update :players #(map (partial user-public-view game) %))
      (update :original-players #(map (partial user-public-view game) %))
      (update :ending-players #(map (partial user-public-view game) %))
      (update :spectators #(map (partial user-public-view game) %))))
