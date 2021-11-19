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
          players (remove #(= uid (:uid %)) (:players lobby))
          spectators (remove #(= uid (:uid %)) (:spectators lobby))]
      (if (pos? (count players))
        (-> lobbies
            (assoc-in [gameid :players] players)
            (assoc-in [gameid :spectators] spectators))
        (dissoc lobbies gameid)))
    lobbies))

(defn uid->lobby [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (into (:players lobby) (:spectators lobby))))
    (vals lobbies)))

(defn uid-player->lobby [lobbies uid]
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

(defn uid-in-lobby?
  ([uid] (uid-in-lobby? uid (:lobbies @app-state)))
  ([uid lobbies]
   (uid->lobby lobbies uid)))

(defn uid-in-lobby-as-player?
  ([uid] (uid-in-lobby-as-player? uid (:lobbies @app-state)))
  ([uid lobbies]
   (uid-player->lobby lobbies uid)))

(defn register-lobby
  [lobbies lobby uid]
  (let [gameid (:gameid lobby)]
    (if (uid-in-lobby-as-player? uid lobbies)
      lobbies
      (assoc lobbies gameid lobby))))

(defn get-lobbies []
  (vals (:lobbies @app-state)))

(defn update-deck-for-player-in-lobby [lobby uid deck]
  (update lobby :players (fn [players]
                           (mapv (fn [p] (if (= uid (:uid p))
                                           (assoc p :deck deck)
                                           p))
                                 players))))

(defn update-deck-for-player [lobbies uid deck]
  (if-let [gameid (:gameid (uid-player->lobby lobbies uid))]
    (update lobbies gameid update-deck-for-player-in-lobby uid deck)
    lobbies))

(defn uid-say [lobbies uid message]
  (if-let [gameid (:gameid (uid->lobby lobbies uid))]
    (update-in lobbies [gameid :messages] conj message)
    lobbies))

;;;;;;;;;;;;;;;; mutations below

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
  [lobby uid]
  (swap! app-state update :lobbies register-lobby lobby uid))

(defn remove-uid-from-lobby!
  "Remove player by uid from participating lobby. Mutates."
  [uid]
  (swap! app-state remove-uid-from-lobby uid))

(defn update-deck-for-player!
  "Update deck for uid in participating lobby. Mutates."
  [uid deck]
  (swap! app-state update :lobbies update-deck-for-player uid deck))

(defn uid-say!
  [uid message]
  (swap! app-state update :lobbies uid-say uid message))
