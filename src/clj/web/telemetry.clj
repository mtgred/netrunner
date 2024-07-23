(ns web.telemetry
  (:require
   [clojure.core.async :refer [<! go timeout]]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [web.app-state :refer [app-state]]
   [web.lobby :refer [lobby-update-uids]]
   [web.ws :refer [connected-sockets connections_]]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]))

(def log-stat-frequency (enc/ms :mins 1))

(defn subscriber-time-metrics
  "average time | oldest"
  [subs]
  (let [now (inst/now)
        age #(duration/get (duration/between % now) chrono/minutes)
        subs-by-minute (sort (map age subs))
        oldest (or (last subs-by-minute) 0)
        average (quot (reduce + 0 subs-by-minute) (max 1 (count subs)))]
    [average oldest]))

(defonce log-stats
  (go (while true
    (<! (timeout log-stat-frequency))
    (let [lobbies-count (count (:lobbies @app-state))
          user-cache-count (count (:users @app-state))
          lobby-sub-count (count (filter identity (vals (:lobby-updates @app-state))))
          lobby-update-uids (count (lobby-update-uids))
          [average-sub-time oldest-sub-time] (subscriber-time-metrics (filter identity (vals (:lobby-updates @app-state))))
          ajax-uid-count (count (:ajax @connected-sockets))
          ajax-conn-counts (seq (map count (:ajax @connections_)))
          ajax-conn-total (reduce + ajax-conn-counts)
          ws-uid-count (count (:ws @connected-sockets))
          ws-conn-counts (seq (map count (:ws @connections_)))
          ws-conn-total (reduce + ws-conn-counts)]
      (timbre/info (str
                     "stats -"
                     " lobbies: " lobbies-count
                     " cached-users: " user-cache-count
                     " lobby-subs: " lobby-sub-count
                     " lobby-update-uids: " lobby-update-uids
                     " average-lobby-subs-lifetime: " average-sub-time "m"
                     " oldest-lobby-sub: " oldest-sub-time "m"
                     " | "
                     "websockets -"
                     " :ajax { "
                     " uid: " ajax-uid-count
                     " conn: " ajax-conn-total
                     " } :ws { "
                     " uid: " ws-uid-count
                     " conn: " ws-conn-total
                     " }"))))))
