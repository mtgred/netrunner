(ns web.app-state
  (:require
   [cljc.java-time.instant :as inst]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [clojure.core.async :refer [<! go timeout]]
   [differ.core :as differ]
   [medley.core :refer [dissoc-in find-first]]
   [taoensso.encore :as enc])
  (:import
   [java.time Instant]
   [java.util.concurrent ConcurrentHashMap]))

(set! *warn-on-reflection* true)

(defonce app-state
  (atom {:lobbies {}
         :lobby-updates {}
         :tournament nil
         :block-game-creation false
         :users {}}))

(defonce last-updates (ConcurrentHashMap/new))

(defn get-last-update [gameid]
  (ConcurrentHashMap/.get last-updates gameid))

(defn set-last-update [gameid]
  (ConcurrentHashMap/.put last-updates gameid (inst/now)))

(defn remove-last-update [gameid]
  (ConcurrentHashMap/.remove last-updates gameid))

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
  "checks if a user receives lobby updates, and updates the state if they've timed out to amortize subsequent checks. Mutates"
  [uid]
  (if-let [last-ping (get-in @app-state [:lobby-updates uid])]
    (if (Instant/.isBefore (inst/now) (inst/plus last-ping lobby-subs-timeout-hours chrono/hours))
      true
      (do (pause-lobby-updates uid) nil))
    (do (pause-lobby-updates uid) nil)))

(defn deregister-user!
  "Remove user from app-state. Mutates."
  [uid]
  (pause-lobby-updates uid)
  (swap! app-state dissoc-in [:users uid]))

(defonce lobby-subs-clearout-freq (enc/ms :mins 5))
(defonce cleanup-lobby-subs
  (go (while true
        (<! (timeout lobby-subs-clearout-freq))
        (doseq [{uid :uid} (vals (:users @app-state))]
          (receive-lobby-updates? uid)))))
