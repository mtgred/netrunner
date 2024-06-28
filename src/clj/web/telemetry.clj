(ns web.telemetry
  (:require
    [clojure.core.async :refer [<! go timeout]]
    [web.app-state :refer [app-state]]
    [web.lobby :refer [lobby-update-uids]]
    [web.ws :refer [connected-sockets connections_]]
    [taoensso.encore :as enc]
    [taoensso.timbre :as timbre]))

(def log-stat-frequency (enc/ms :mins 5))

(defonce log-stats
  (go (while true
    (<! (timeout log-stat-frequency))
    (let [lobbies-count (count (:lobbies @app-state))
          user-cache-count (count (:users @app-state))
          lobby-sub-count (count (filter identity (vals (:lobby-updates @app-state))))
          lobby-update-uids (count (lobby-update-uids))
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
                     " | "
                     "websockets -"
                     " :ajax { "
                     " uid: " ajax-uid-count
                     " conn: " ajax-conn-total
                     " } :ws { "
                     " uid: " ws-uid-count
                     " conn: " ws-conn-total
                     " }"))))))
