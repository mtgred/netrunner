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

(defn contains-key?
  [m ks]
  (not= ::absent (get m ks ::absent)))

(defn update-if-contains
  [m ks f & args]
  (if (contains-key? m ks)
    (apply (partial update m ks f) args)
    m))

(defn game-internal-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [game]
  (-> game
      (dissoc :state :last-update :on-close)
      (update-if-contains :players #(map (partial user-public-view game) %))
      (update-if-contains :original-players #(map (partial user-public-view game) %))
      (update-if-contains :spectators #(map (partial user-public-view game) %))
      (update-if-contains :ending-players #(map (partial user-public-view game) %))))

(defn game-public-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [game]
  (game-internal-view (dissoc game :messages :spectators)))

(defn game-lobby-view
  "Strips private server information from a game map, preparing to send the game to clients. Strips messages in addition to private info to keep payload size down"
  [game]
  (game-internal-view (select-keys game [:messages :spectators])))
