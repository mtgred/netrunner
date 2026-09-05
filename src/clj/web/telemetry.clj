(ns web.telemetry
  (:require
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [clojure.core.async :refer [alt! chan close! go-loop put! timeout]]
   [game.core.board :refer [all-active]]
   [integrant.core :as ig]
   [taoensso.carmine :as car :refer [wcar]]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]
   [web.app-state :refer [app-state]]
   [web.lobby :refer [fetch-delay-log! lobby-update-uids pool-occupants-info]]
   [web.ws :as ws]))

(defn percentile [vector percentile]
  ;; see: https://scicloj.github.io/stats-with-clojure/stats_with_clojure.basic_statistics.html
  (let [sorted-vector (sort vector)]
    (if (empty? sorted-vector)
      "0ms"
      (let [idx (min (int (* percentile (/ (count sorted-vector) 100)))
                     (dec (count sorted-vector)))]
        (str (nth sorted-vector idx) "ms")))))

(defn- format-percentiles [data percentiles]
  (apply str (interpose "/" (map #(percentile (vec data) %) percentiles))))

(defn format-delay! []
  (let [delays (fetch-delay-log!)
        percentiles [5 25 50 75 95]
        av #(quot (reduce + 0 %) (count %))
        fmt #(str "Average: " (av %) "ms - Count: " (count %) " - Percentiles (5/25/50/75/95): "
                  (format-percentiles % percentiles))]
    (str (update-vals delays fmt))))

(defn subscriber-time-metrics
  "average time | oldest"
  [subs]
  (let [now (inst/now)
        age #(quot (duration/get (duration/between % now) chrono/seconds) 60)
        subs-by-minute (sort (map age subs))
        oldest (or (last subs-by-minute) 0)
        average (quot (reduce + 0 subs-by-minute) (max 1 (count subs)))]
    [average oldest]))

(defn- lobby->active-cards
  [lobby]
  (if (:started lobby)
    (let [state (:state lobby)]
      (map :title (concat (all-active state :runner) (all-active state :corp))))
    []))

(defn active-card-frequencies
  [lobbies]
  (frequencies (reduce concat [] (map lobby->active-cards (vals lobbies)))))

(defn lobby->recent-commands
  [lobby]
  (if (:started lobby)
    (let [state (:state lobby)
          commands (or (:command-log @state) [])
          recent (filter #(inst/is-after (inst/plus-seconds (:timestamp %) (* 5 60)) (inst/now))
                         commands)]
      (map :command recent))
    []))

(defn recent-command-frequencies
  [lobbies]
  (frequencies (reduce concat [] (map lobby->recent-commands (vals lobbies)))))

(defn- heap-usage
  []
  (.getHeapMemoryUsage (java.lang.management.ManagementFactory/getMemoryMXBean)))

(defn- system-load-average
  []
  (let [bean (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (str (int (* 100 (/ (.getSystemLoadAverage bean) (.getAvailableProcessors bean)))) "%")))

(defn thread-stats []
  (let [threads (Thread/getAllStackTraces)]
    (frequencies (map #(keyword (.name (.getState %))) (keys threads)))))

(defn ws-chan-backlog [ws]
  (let [{:keys [pending size]} (ws/buffer-stats ws)]
    (str "websocket-buffer: " pending " / " size)))

(def last-gc-stats (atom {}))
(defn log-gc []
  (let [gc-beans (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)
        current (into {}
                      (map (fn [gc]
                             [(.getName gc)
                              {:collections (.getCollectionCount gc)
                               :time (.getCollectionTime gc)}])
                           gc-beans))]
    ;; Calculate the deltas since last log
    (doseq [[gc-name {:keys [collections time]}] current]
      (let [prev (get @last-gc-stats gc-name {:collections 0 :time 0})
            delta-collections (- collections (:collections prev))
            delta-time (- time (:time prev))]
        ;; Log total collections and total time collected since last log
        (timbre/info (format "GC '%s': Collections = %d, Time (ms) = %d"
                             gc-name delta-collections delta-time))))
    ;; Update last-gc-stats for future comparison
    (reset! last-gc-stats current)))

(defn log-open-file-descriptors []
  (let [os-bean (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? com.sun.management.UnixOperatingSystemMXBean os-bean)
      (let [open (.getOpenFileDescriptorCount ^com.sun.management.UnixOperatingSystemMXBean os-bean)
            max (.getMaxFileDescriptorCount ^com.sun.management.UnixOperatingSystemMXBean os-bean)]
        (timbre/info
          (str "Open file descriptors: "
               (format "%d / %d (%.1f%%)" open max (* 100.0 (/ open max))))))
      (timbre/info "Warning: Open FD count not supported on this JVM"))))

(defn log-stats
  [redis ws]
  (timbre/with-context+
    {:type :telemetry}
    (let [lobbies (:lobbies @app-state)
          lobbies-count (count lobbies)
          players (reduce + 0 (map #(count (:players %)) (vals lobbies)))
          spectators (reduce + 0 (map #(count (:spectators %)) (vals lobbies)))
          ; card-freqs (active-card-frequencies lobbies)
          ; cmd-freqs (recent-command-frequencies lobbies)
          user-cache-count (count (:users @app-state))
          lobby-updates (vals (wcar redis (car/parse-map (car/hgetall :lobby-updates))))
          lobby-sub-count (count lobby-updates)
          lobby-update-uids (count (lobby-update-uids redis ws))
          [average-sub-time oldest-sub-time] (subscriber-time-metrics lobby-updates)
          latencies (format-delay!)
          ajax-uid-count (count (:ajax (ws/connected-sockets ws)))
          ajax-conn-counts (seq (map count (:ajax (ws/connections ws))))
          ajax-conn-total (reduce + ajax-conn-counts)
          ws-uid-count (count (:ws (ws/connected-sockets ws)))
          ws-conn-counts (seq (map count (:ws (ws/connections ws))))
          ws-conn-total (reduce + ws-conn-counts)]
      (timbre/info (str
                    "stats -"
                    " lobbies: " lobbies-count
                    " players: " players
                    " spectators: " spectators
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
                    " }"))
      (timbre/info "pool occupants:" (seq (map count (pool-occupants-info))))
      (timbre/info latencies)
      ;; note: the two below (active cards and recent commands) are not relevant for our current situation I think
      ;; if we ever get locking issues or something in the future, it can be useful to diagnose them though
      ;;(timbre/info (str "Active Cards (across all lobbies): " card-freqs))
      ;;(timbre/info (str "Recent Commands (across all lobbies): " cmd-freqs))
      (timbre/info "thread states:" (thread-stats))
      (timbre/info (ws-chan-backlog ws))
      (log-gc)
      (log-open-file-descriptors)
      (timbre/info (str "System Load (average): " (system-load-average)
                        " - heap: " (heap-usage) "\n")))))

(defmethod ig/init-key :web/telemetry
  [_ {:web/keys [redis ws]
      :keys [frequency]
      :or {frequency {:mins 5}}}]
  (let [exit-ch (chan)
        log-stat-frequency (enc/ms frequency)]
    (go-loop []
      (let [timeout-ch (timeout log-stat-frequency)]
        (alt!
          exit-ch nil
          timeout-ch (log-stats redis ws)))
      (recur))
    exit-ch))

(defmethod ig/halt-key! :web/telemetry [_ stop-ch]
  (put! stop-ch true)
  (close! stop-ch)
  nil)
