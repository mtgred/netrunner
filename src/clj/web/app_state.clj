(ns web.app-state
  (:require
    [medley.core :refer [find-first]]))

(defonce app-state
  (atom {:lobbies {}
         :lobby-updates {}
         :users {}}))

(defn register-user
  [app-state uid user]
  (-> app-state
      (assoc-in [:users uid] (assoc user :uid uid))
      (assoc-in [:lobby-updates uid] true)))

(defn uid->lobby
  ([uid] (uid->lobby (:lobbies @app-state) uid))
  ([lobbies uid]
   (find-first
     (fn [lobby]
       (some #(= uid (:uid %)) (into (:players lobby) (:spectators lobby))))
     (vals lobbies))))

(defn uid-player->lobby
  ([uid] (uid-player->lobby (:lobbies @app-state) uid))
  ([lobbies uid]
    (find-first
      (fn [lobby]
        (some #(= uid (:uid %)) (:players lobby)))
      (vals lobbies))))

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

(defn deregister-user!
  "Remove user from app-state. Mutates."
  [uid]
  (let [users (:users @app-state)
        new-users (dissoc users uid)]
    (swap! app-state #(assoc %1 :users new-users))))

(defn pause-lobby-updates
  [uid]
  (swap! app-state assoc-in [:lobby-updates uid] false))

(defn continue-lobby-updates
  [uid]
  (swap! app-state assoc-in [:lobby-updates uid] true))

(defn receive-lobby-updates?
  [uid]
  (get-in @app-state [:lobby-updates uid] false))
