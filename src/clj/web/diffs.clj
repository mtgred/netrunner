(ns web.diffs)

(defn- filter-lobby-user
  "Only take keys that are useful in the lobby from a user map"
  [user]
  (let [options (select-keys (:options user) [:blocked-users])
        stats (select-keys (:stats user) [:games-started :games-completed])
        filtered-user (select-keys user [:_id :username :emailhash])]
    (assoc filtered-user :options options :stats stats)))

(defn- user-public-view
  "Strips private server information from a player map."
  [{:keys [started format]} player]
  (as-> player p
    (dissoc p :ws-id)
    (assoc p :user (filter-lobby-user (:user p)))
    (if-let [{:keys [_id] :as deck} (:deck p)]
      (let [legal (get-in deck [:status (keyword format) :legal])
            status {:format format
                    (keyword format) {:legal legal}}
            deck (-> deck
                     (select-keys
                       (if started
                         [:name :date :identity :hash]
                         [:name :date]))
                     (assoc :_id (str _id) :status status))]
        (assoc p :deck deck))
      p)))

(defn- update-if-contains
  [m ks f & args]
  (if (get m ks)
    (apply (partial update m ks f) args)
    m))

(defn game-internal-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [full-game game-update]
  (-> game-update
    (dissoc :state :last-update :last-update-only-actions :on-close)
    (update-if-contains :password (fn [_] true))
    (update-if-contains :players #(map (partial user-public-view full-game) %))
    (update-if-contains :original-players #(map (partial user-public-view full-game) %))
    (update-if-contains :spectators #(map (partial user-public-view full-game) %))
    (update-if-contains :ending-players #(map (partial user-public-view full-game) %))))
