(ns web.app-state
  (:require
    [executor.db :as db]
    [medley.core :refer [find-first]]))

(defonce app-state db/app-db)

(defn register-user
  [app-state uid user]
  (assoc-in app-state [:users uid] (assoc user :uid uid)))

(defn uid->lobby
  ([uid] (uid->lobby (:lobbies @app-state) uid))
  ([lobbies uid]
   (find-first
     (fn [lobby]
       (some #(= uid (:uid %)) (into (:players lobby) (:spectators lobby))))
     (vals lobbies))))

(defn uid-player->lobby [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (:players lobby)))
    (vals lobbies)))

(defn get-users []
  (vals (:users @app-state)))

(defn get-user [uid]
  (get-in @app-state [:users uid]))

(defn uid-in-lobby-as-player?
  ([uid] (uid-in-lobby-as-player? uid (:lobbies @app-state)))
  ([uid lobbies]
   (uid-player->lobby lobbies uid)))

(defn get-lobbies []
  (vals (:lobbies @app-state)))

(defn get-lobby
  ([gameid] (get-lobby gameid (:lobbies @app-state)))
  ([gameid lobbies] (get lobbies gameid)))

(defn register-user!
  "Add user to uid in app-state. Mutates."
  [uid user]
  (swap! app-state register-user uid user))
