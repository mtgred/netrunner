(ns web.app-state
  (:require
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljc.java-time.instant :as inst]
   [medley.core :refer [dissoc-in find-first]]))

(defonce app-state
  (atom {:lobbies {}
         :lobby-updates {}
         :users {}}))

(defonce lobby-subs-timeout-hours 6)

(defn register-user
  [app-state uid user]
  (-> app-state
      (assoc-in [:users uid] (assoc user :uid uid))
      (assoc-in [:lobby-updates uid] (inst/now))))

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


(defn pause-lobby-updates
  [uid]
  (swap! app-state dissoc-in [:lobby-updates uid]))

(defn continue-lobby-updates
  [uid]
  (swap! app-state assoc-in [:lobby-updates uid] (inst/now)))

(defn receive-lobby-updates?
  [uid]
  (if-let [last-ping (get-in @app-state [:lobby-updates uid])]
    (.isBefore (inst/now) (inst/plus last-ping lobby-subs-timeout-hours chrono/hours))
    (do (pause-lobby-updates uid)
        nil)))

(defn deregister-user!
  "Remove user from app-state. Mutates."
  [uid]
  (pause-lobby-updates uid)
  (swap! app-state dissoc-in [:users uid]))
