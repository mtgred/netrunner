(ns web.app-state
  (:require
   [cljc.java-time.instant :as inst]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [medley.core :refer [dissoc-in find-first]]
   [taoensso.carmine :as car :refer [wcar]])
  (:import
   [java.time Instant]))

(set! *warn-on-reflection* true)

(def base-app-state
  {:lobbies {}
   :tournament nil
   :block-game-creation false
   :users {}})

(defonce app-state
  (atom base-app-state))

(defn get-last-update [redis gameid]
  (wcar redis (car/hget :last-update gameid)))

(defn set-last-update [redis gameid]
  (wcar redis (car/hset :last-update gameid (inst/now))))

(defn remove-last-update [redis gameid]
  (wcar redis (car/hdel :last-update gameid)))

(defonce lobby-subs-timeout-hours 1)

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

(defn tournament-state []
  (:tournament @app-state nil))

(defn get-lobby
  ([gameid] (get-lobby gameid (:lobbies @app-state)))
  ([gameid lobbies] (get lobbies gameid)))

(defn pause-lobby-updates
  [redis uid]
  (wcar redis (car/hdel :lobby-updates uid))
  nil)

(defn receive-lobby-updates
  [redis uid]
  (wcar redis (car/hset :lobby-updates uid (inst/now)))
  nil)

(defn receive-lobby-updates?
  "checks if a user receives lobby updates, and updates the state if they've timed out to amortize subsequent checks. Mutates"
  [redis uid]
  (if-let [last-ping (wcar redis (car/hget :lobby-updates uid))]
    (if (Instant/.isBefore (inst/now) (inst/plus last-ping lobby-subs-timeout-hours chrono/hours))
      true
      (pause-lobby-updates redis uid))
    (pause-lobby-updates redis uid)))

(defn register-user!
  "Add user to uid in app-state. Mutates."
  [redis uid user]
  (swap! app-state register-user uid user)
  (receive-lobby-updates redis uid)
  nil)

(defn deregister-user!
  "Remove user from app-state. Mutates."
  [{:system/keys [redis]} uid]
  (swap! app-state dissoc-in [:users uid])
  (pause-lobby-updates redis uid)
  nil)
