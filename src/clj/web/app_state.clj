(ns web.app-state
  (:require
    [medley.core :refer [find-first]]))

(defonce app-state
  (atom {:lobbies {}
         :users {}}))

(defn register-user
  [app-state uid user]
  (assoc-in app-state [:users uid] (assoc user :uid uid)))

(defn remove-uid-from-lobbies
  [lobbies gameid uid]
  (if gameid
    (let [lobby (get lobbies gameid)
          players (remove #(= uid (:uid %)) (:players lobby))]
      (if (pos? (count players))
        (assoc-in lobbies [gameid :players] players)
        (dissoc lobbies gameid)))
    lobbies))

(defn uid->lobby [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (:players lobby)))
    (vals lobbies)))

(defn remove-uid-from-lobby
  [{:keys [lobbies users]} uid]
  (let [gameid (:gameid (uid->lobby lobbies uid))
        lobbies (remove-uid-from-lobbies lobbies gameid uid)]
    {:lobbies lobbies
     :users users}))

(defn unregister-user
  [{:keys [lobbies users]} uid]
  (let [gameid (:gameid (uid->lobby lobbies uid))
        lobbies (remove-uid-from-lobbies lobbies gameid uid)]
    {:lobbies lobbies
     :users (dissoc users uid)}))

(defn get-users []
  (vals (:users @app-state)))

(defn get-user [uid]
  (get-in @app-state [:users uid]))

(defn uid-in-lobby? [uid]
  (uid->lobby (:lobbies @app-state) uid))

(defn register-lobby
  [{:keys [lobbies users]} lobby]
  (let [gameid (:gameid lobby)
        lobbies (assoc lobbies gameid lobby)]
    {:lobbies lobbies
     :users users}))

(defn unregister-lobby
  [{:keys [lobbies users]} lobby]
  (let [gameid (:gameid lobby)
        lobbies (dissoc lobbies gameid)]
    {:lobbies lobbies
     :users users}))

(defn get-lobbies []
  (vals (:lobbies @app-state)))

(defn update-deck-for-player-in-lobby [lobby uid deck]
  (update lobby :players (fn [players]
                           (mapv (fn [p] (if (= uid (:uid p))
                                           (assoc p :deck deck)
                                           p))
                                 players))))

(defn update-deck-for-player [lobbies uid deck]
  (if-let [gameid (:gameid (uid->lobby lobbies uid))]
    (update lobbies gameid update-deck-for-player-in-lobby uid deck)
    lobbies))

(defn register-user!
  "Add user to uid in app-state. Mutates."
  [uid user]
  (swap! app-state register-user uid user))

(defn unregister-user!
  "Remove uid from app-state (including from a joined lobby). Mutates."
  [uid]
  (swap! app-state unregister-user uid))

(defn register-lobby!
  "Add lobby to gameid in app-state. Mutates."
  [lobby]
  (swap! app-state register-lobby lobby))

(defn unregister-lobby!
  "Remove lobby from app-state. Mutates."
  [lobby]
  (swap! app-state unregister-lobby lobby))

(defn remove-uid-from-lobby!
  "Remove player by uid from participating lobby. Mutates."
  [uid]
  (swap! app-state remove-uid-from-lobby uid))

(defn update-deck-for-player!
  "Update deck for uid in participating lobby. Mutates."
  [uid deck]
  (swap! app-state update :lobbies update-deck-for-player uid deck))
