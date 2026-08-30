(ns tasks.stress.run
  "Stress test: runs N concurrent bot-vs-bot games against a running server
  over real websockets and samples the server's resource usage over time.

  See `bin/stress-test --help` for usage."
  (:require
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [jinteki.preconstructed :refer [all-matchups]]
    [tasks.stress.bot :as bot]
    [tasks.stress.client :as client]
    [tasks.stress.profiler :as profiler])
  (:import
    (java.time Instant)
    (java.time.temporal ChronoUnit)))

(defn- now-ms [] (System/currentTimeMillis))

;; game lifecycle

(defn- locked? [c]
  (when-let [{:keys [sent-at]} @(:pending-action c)]
    (< (- (System/nanoTime) sent-at) 5e9)))

(def ^:private chat-phrases
  ["thinking..." "gl hf" "one sec" "nice" "oops" "well played" "hm"])

(defn- act! [c side gameid ^java.util.Random rng {:keys [chat-chance]}]
  (if (and (pos? chat-chance) (< (.nextDouble rng) chat-chance))
    (client/send-chat! c gameid (nth chat-phrases (.nextInt rng (count chat-phrases))))
    (when-not (locked? c)
      (when-let [[command args] (bot/decide @(:game-state c) side rng)]
        (client/send-action! c side gameid command args)))))

(defn- last-diff-at [slot]
  (max @(:last-event-at (:corp slot)) @(:last-event-at (:runner slot))))

(defn- winner [slot]
  (some :winner [@(:game-state (:corp slot)) @(:game-state (:runner slot))]))

(defn- jitter [^java.util.Random rng delay-ms]
  (long (* delay-ms (+ 0.7 (* 0.6 (.nextDouble rng))))))

(defn- clear-states! [slot]
  (doseq [c (list* (:corp slot) (:runner slot) (:spectators slot))]
    (reset! (:game-state c) nil)
    (reset! (:lobby-state c) nil)
    (reset! (:pending-action c) nil)))

(defn- leave-game! [slot gameid]
  (doseq [c (list* (:corp slot) (:runner slot) (:spectators slot))]
    (client/send-event! c [:game/leave {:gameid gameid}])
    ;; back on the lobby page between games, like the browser
    (client/send-event! c [:lobby/continue-updates]))
  (clear-states! slot))

(defn- abandon-stale-lobby!
  "After an unclean shutdown the server may still have this user in a lobby,
  which makes it silently refuse to create a new one. Ask where we are and leave."
  [c]
  (reset! (:lobby-state c) nil)
  (client/send-event! c [:lobby/list])
  (when-let [gameid (:gameid (client/wait-for #(deref (:lobby-state c)) 3000))]
    (client/send-event! c [:game/leave {:gameid gameid}])
    (client/send-event! c [:lobby/leave {:gameid gameid}])
    (client/wait-for #(nil? (:gameid @(:lobby-state c))) 5000)
    (println (:username c) "left a stale lobby")))

(defn- start-game!
  "Creates a lobby, joins, starts the game and connects spectators.
  Returns the gameid. Throws on timeout."
  [{:keys [corp runner spectators id] :as slot} matchup save-replays?]
  (doseq [c (list* corp runner spectators)]
    (abandon-stale-lobby! c))
  (clear-states! slot)
  (client/send-event! corp [:lobby/create {:title (str "stress-" id)
                                           :format "preconstructed"
                                           :precon (name matchup)
                                           :room "casual"
                                           :side "Corp"
                                           :allow-spectator true
                                           :spectatorhands false
                                           :save-replay save-replays?
                                           :password ""
                                           :options {}}])
  (let [gameid (client/wait-for #(:gameid @(:lobby-state corp)) 30000)]
    (when-not gameid
      (throw (ex-info "lobby create timed out" {:slot id})))
    (try
      (client/send-event! runner [:lobby/join {:gameid gameid :password ""}])
      (when-not (client/wait-for #(= gameid (:gameid @(:lobby-state runner))) 30000)
        (throw (ex-info "lobby join timed out" {:slot id})))
      (client/send-event! corp [:game/start {:gameid gameid}])
      (when-not (client/wait-for #(and @(:game-state corp) @(:game-state runner)) 60000)
        (throw (ex-info "game start timed out" {:slot id})))
      (doseq [spec spectators]
        (client/send-event! spec [:game/watch {:gameid gameid :password ""}]))
      ;; the browser pauses lobby updates once you're in a game; mirror it so
      ;; the broadcast audience is watchers plus whoever is between games
      (doseq [c (list* corp runner spectators)]
        (client/send-event! c [:lobby/pause-updates]))
      gameid
      (catch Exception e
        ;; leave the half-made lobby or the server will refuse our next create
        (client/send-event! corp [:lobby/leave {:gameid gameid}])
        (throw e)))))

(defn- play-game!
  "Runs the bot loop until the game has a winner, wedges, or the run stops."
  [{:keys [corp runner rng] :as slot} gameid {:keys [delay-ms wedge-timeout-ms stop? counters] :as ctx}]
  (let [game-deadline (+ (now-ms) (* 20 60 1000))
        concede! (fn []
                   (swap! counters update :wedges inc)
                   (client/send-action! corp :corp gameid "concede" nil)
                   (client/wait-for #(winner slot) 10000))]
    (loop []
      (Thread/sleep (jitter rng delay-ms))
      (cond
        @stop?
        (client/send-action! corp :corp gameid "concede" nil)

        (winner slot)
        (swap! counters update :games-completed inc)

        (or @(:reconnected? corp) @(:reconnected? runner))
        (throw (ex-info "websocket reconnected, game lost" {:slot (:id slot)}))

        (or (> (- (now-ms) (last-diff-at slot)) wedge-timeout-ms)
            (> (now-ms) game-deadline))
        (concede!)

        :else
        (do (act! corp :corp gameid rng ctx)
            (act! runner :runner gameid rng ctx)
            (recur))))))

(defn- blocked-usernames
  "0..max-blocks players from other games, never the own opponent, so the
  pairings this slot plays are unaffected while playing players still block
  each other across games."
  [games max-blocks id role]
  (let [n (mod (+ id (case role :corp 0 :runner 1)) (inc max-blocks))]
    (vec (distinct
          (for [k (range 1 (inc n))
                :let [target (mod (+ id k) games)]
                :when (not= target id)]
            (str (case role :corp "stress-runner-" :runner "stress-corp-") target))))))

(defn- watcher-blocked-usernames
  "Watchers block 0..max-blocks playing players, exercising the lobby list
  filter's slow path for part of the crowd."
  [games max-blocks id]
  (let [n (mod id (inc max-blocks))]
    (vec (distinct
          (for [k (range 1 (inc n))]
            (str (if (even? k) "stress-corp-" "stress-runner-")
                 (mod (+ id k) (max games 1))))))))

(defn- pre-authenticate!
  "Registers/logs in every user of the run and writes their block lists.
  Logging in again is rare in production (sessions persist), so this
  bcrypt-heavy stage runs before profiling starts. Returns username->auth."
  [{:keys [base-url games max-blocks spectators-per-game]} lobby-watchers]
  (let [users (concat
               (mapcat (fn [id]
                         [[(str "stress-corp-" id) (blocked-usernames games max-blocks id :corp)]
                          [(str "stress-runner-" id) (blocked-usernames games max-blocks id :runner)]])
                       (range games))
               (for [id (range games)
                     s (range spectators-per-game)]
                 [(str "stress-spec-" id "-" s) []])
               (for [i (range lobby-watchers)]
                 [(str "stress-watch-" i) (watcher-blocked-usernames games max-blocks i)]))]
    (println (format "Authenticating %d users..." (count users)))
    (let [auths (->> users
                     (pmap (fn [[username blocked]]
                             (let [auth (try
                                          (client/login! base-url username "stress-password")
                                          (catch Exception _
                                            (Thread/sleep 1000)
                                            (client/login! base-url username "stress-password")))]
                               (client/set-blocked-users! base-url auth blocked)
                               [username auth])))
                     (into {}))]
      (println "Authentication done")
      auths)))

(defn- connect-watchers!
  "Connects idle users subscribed to lobby updates: the tournament crowd that
  receives every lobby-list broadcast without playing or spectating."
  [{:keys [base-url counters auths stop?]} n]
  (println (format "Connecting %d lobby watchers..." n))
  (let [watchers (->> (range n)
                      (pmap (fn [i]
                              (try
                                (let [username (str "stress-watch-" i)
                                      c (client/connect! base-url (get auths username) username counters)]
                                  ;; subscribe explicitly; this also delivers a first list
                                  (client/send-event! c [:lobby/continue-updates])
                                  c)
                                (catch Exception e
                                  (println "watcher" i "failed:" (.getMessage e))
                                  nil))))
                      (filterv some?))]
    ;; subscriptions lapse after an hour; refresh like a browser on the lobby page
    (doto (Thread.
           (fn []
             (loop []
               (Thread/sleep 600000)
               (when-not @stop?
                 (doseq [c watchers]
                   (try (client/send-event! c [:lobby/continue-updates]) (catch Exception _)))
                 (recur))))
           "stress-watchers-keepalive")
      (.setDaemon true)
      (.start))
    (println (format "%d lobby watchers connected" (count watchers)))
    watchers))

(defn- connect-slot!
  "(Re)connects all clients of a slot; on failure disconnects any partial ones.
  Auth and block lists come from the pre-authentication stage; a fresh login
  only happens as a fallback (e.g. an expired session on a very long run)."
  [{:keys [base-url counters auths]} id spectators-per-game]
  (let [connected (atom [])
        conn (fn [username]
               (let [auth (or (get auths username)
                              (client/login! base-url username "stress-password"))
                     c (client/connect! base-url auth username counters)]
                 ;; a browser lands on the lobby page and subscribes itself;
                 ;; the server no longer subscribes anyone by default
                 (client/send-event! c [:lobby/continue-updates])
                 (swap! connected conj c)
                 c))]
    (try
      {:id id
       :rng (java.util.Random. (+ 42 id))
       :corp (conn (str "stress-corp-" id))
       :runner (conn (str "stress-runner-" id))
       :spectators (mapv #(conn (str "stress-spec-" id "-" %)) (range spectators-per-game))}
      (catch Exception e
        (doseq [c @connected]
          (try (client/disconnect! c) (catch Exception _)))
        (throw e)))))

(defn- disconnect-slot! [slot]
  (doseq [c (list* (:corp slot) (:runner slot) (:spectators slot))]
    (try (client/disconnect! c) (catch Exception _))))

(defn- slot-loop
  "One thread per slot: play games back to back until stopped."
  [{:keys [stop? counters active-games matchups spectators-per-game save-replays?] :as ctx} id]
  (Thread/sleep (* 200 id))
  (loop [slot nil
         [matchup & more] (drop id (cycle matchups))]
    (let [slot' (try
                  (let [slot (or slot (connect-slot! ctx id spectators-per-game))
                        gameid (start-game! slot matchup save-replays?)]
                    (swap! active-games inc)
                    (try
                      (play-game! slot gameid ctx)
                      (finally
                        (swap! active-games dec)
                        (leave-game! slot gameid)))
                    slot)
                  (catch InterruptedException _ nil)
                  (catch Exception e
                    (swap! counters update :slot-resets inc)
                    (println (str "slot " id " reset: " (.getMessage e)))
                    (when slot (disconnect-slot! slot))
                    (when-not @stop?
                      (Thread/sleep 3000))
                    nil))]
      (if @stop?
        (when slot' (disconnect-slot! slot'))
        (recur slot' more)))))

;; resource sampling

(defn- parse-mem [s]
  (when-let [[_ n unit] (re-find #"([\d.]+)\s*([KMGT]?i?B)" (str s))]
    (long (* (Double/parseDouble n)
             (case (first unit)
               \K 1024 \M (* 1024 1024) \G (* 1024 1024 1024) \T (* 1024 1024 1024 1024)
               1)))))

(defn- docker-sampler!
  "Streams `docker stats` into the latest-sample atom until stopped."
  [container latest stop?]
  (let [proc (.start (ProcessBuilder. ["docker" "stats" "--format" "{{json .}}" container]))]
    (doto (Thread.
           (fn []
             (with-open [rdr (io/reader (.getInputStream proc))]
               ;; the first streamed frame reports stale values, skip it
               (doseq [[i line] (map-indexed vector (line-seq rdr))
                       :while (not @stop?)
                       :when (pos? i)]
                 (when-let [start (str/index-of line "{")]
                   (try
                     (let [sample (json/parse-string (subs line start) true)]
                       (reset! latest {:cpu-pct (some-> (:CPUPerc sample) (str/replace "%" "") Double/parseDouble)
                                       :mem-bytes (parse-mem (:MemUsage sample))}))
                     (catch Exception _))))))
           "docker-sampler")
      (.setDaemon true)
      (.start))
    proc))

(defn- ps-sample [pid]
  (let [out (:out (shell/sh "ps" "-o" "%cpu=,rss=" "-p" (str pid)))
        [cpu rss] (some-> out str/trim (str/split #"\s+"))]
    (when (and cpu rss)
      {:cpu-pct (Double/parseDouble cpu)
       :mem-bytes (* 1024 (Long/parseLong rss))})))

(defn- percentile [sorted p]
  (when (seq sorted)
    (nth sorted (min (dec (count sorted)) (int (* p (count sorted)))))))

(defn- sample-row [t0 latest-resources counters-snapshot prev-snapshot active-games lats]
  (let [{:keys [cpu-pct mem-bytes]} @latest-resources
        sorted (vec (sort lats))]
    {:elapsed-s (quot (- (now-ms) t0) 1000)
     :cpu-pct cpu-pct
     :mem-mb (some-> mem-bytes (quot (* 1024 1024)))
     :actions (- (:actions counters-snapshot 0) (:actions prev-snapshot 0))
     :diffs (- (:diffs counters-snapshot 0) (:diffs prev-snapshot 0))
     :errors (:errors counters-snapshot 0)
     :active-games @active-games
     :games-completed (:games-completed counters-snapshot 0)
     :wedges (:wedges counters-snapshot 0)
     :slot-resets (:slot-resets counters-snapshot 0)
     :chats (:chats counters-snapshot 0)
     :lobby-lists (:lobby-lists counters-snapshot 0)
     :lat-p50-ms (percentile sorted 0.50)
     :lat-p95-ms (percentile sorted 0.95)
     :lat-p99-ms (percentile sorted 0.99)}))

(def ^:private csv-columns
  [:elapsed-s :cpu-pct :mem-mb :actions :diffs :errors :active-games
   :games-completed :wedges :slot-resets :chats :lobby-lists
   :lat-p50-ms :lat-p95-ms :lat-p99-ms])

(defn- fmt [x]
  (cond
    (nil? x) ""
    (double? x) (format "%.2f" x)
    :else (str x)))

(defn- status-line [{:keys [elapsed-s cpu-pct mem-mb actions active-games errors lat-p95-ms]}]
  (format "[%4ds] cpu %s%% mem %sMB | games %d | %s act/s | p95 %sms | errors %s"
          elapsed-s (fmt cpu-pct) (fmt mem-mb) active-games (fmt actions) (fmt lat-p95-ms) (fmt errors)))

(defn- summarize [rows all-latencies config]
  (let [cpus (keep :cpu-pct rows)
        mems (keep :mem-mb rows)
        sorted (vec (sort all-latencies))
        total-actions (reduce + 0 (keep :actions rows))
        duration (max 1 (- (:elapsed-s (last rows) 1) (:elapsed-s (first rows) 0)))]
    {:config config
     :run-at (str (Instant/now))
     :duration-s duration
     :cpu-pct {:mean (when (seq cpus) (/ (reduce + cpus) (count cpus)))
               :max (when (seq cpus) (apply max cpus))}
     :mem-mb {:mean (when (seq mems) (quot (reduce + mems) (count mems)))
              :max (when (seq mems) (apply max mems))}
     :actions {:total total-actions :per-s (double (/ total-actions duration))}
     :chats (:chats (last rows) 0)
     :lobby-lists (:lobby-lists (last rows) 0)
     :action-latency-ms {:p50 (percentile sorted 0.50)
                         :p95 (percentile sorted 0.95)
                         :p99 (percentile sorted 0.99)
                         :max (last sorted)}
     :games-completed (:games-completed (last rows) 0)
     :wedges (:wedges (last rows) 0)
     :slot-resets (:slot-resets (last rows) 0)
     :errors (:errors (last rows) 0)}))

;; entry point

;; defaults are sized for a local dev run: enough concurrent games for
;; meaningful contention, long enough for several games to complete
(def ^:private cli-options
  [["-n" "--concurrent-games N" "Number of games running at the same time"
    :default 10 :parse-fn #(Integer/parseInt %) :validate [pos? "must be positive"]]
   ["-d" "--duration-seconds SEC" "How long to keep the load going"
    :default 180 :parse-fn #(Integer/parseInt %) :validate [pos? "must be positive"]]
   [nil "--delay MS" "Think time per bot between actions (lower = more load)"
    :default 1000 :parse-fn #(Integer/parseInt %) :validate [#(>= % 50) "must be >= 50"]]
   [nil "--matchups LIST" "Comma-separated preconstructed matchup keys (see jinteki.preconstructed), or 'all'"
    :default "all"]
   [nil "--spectators N" "Spectators per game"
    :default 0 :parse-fn #(Integer/parseInt %)]
   [nil "--save-replays" "Save replays at game end, like tournament games do"
    :default false]
   [nil "--chat-chance P" "Probability (0-1) a bot chats in-game instead of acting on a tick"
    :default 0.0 :parse-fn #(Double/parseDouble %)
    :validate [#(<= 0.0 % 1.0) "must be between 0 and 1"]]
   [nil "--max-blocks N" "Players block 0..N players from other games (0 disables)"
    :default 0 :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 10) "must be between 0 and 10"]]
   [nil "--lobby-watchers N" "Idle connected users subscribed to lobby updates (the tournament crowd)"
    :default 0 :parse-fn #(Integer/parseInt %)]
   [nil "--profile" "Capture a profile of the server into the output directory"
    :default false]
   [nil "--profile-event EVENT" "What to profile: cpu, alloc (allocation pressure), wall, or itimer"
    :default "cpu" :validate [#{"cpu" "alloc" "wall" "itimer"} "must be cpu, alloc, wall or itimer"]]
   [nil "--nrepl-port PORT" "Server nREPL port, used by --profile"
    :default 44867 :parse-fn #(Integer/parseInt %)]
   [nil "--url URL" "Server base url" :default "http://localhost:1042"]
   [nil "--container NAME" "Docker container to sample resources from"
    :default "netrunner-server-1"]
   [nil "--pid PID" "Sample this OS pid with ps instead of docker stats"
    :parse-fn #(Long/parseLong %)]
   [nil "--out DIR" "Output directory (default stress-runs/<timestamp>)"]
   ["-h" "--help"]])

(defn- resolve-matchups [s]
  (if (= "all" s)
    (vec (sort all-matchups))
    (let [ks (mapv keyword (str/split s #","))
          bad (remove all-matchups ks)]
      (when (seq bad)
        (throw (ex-info (str "unknown matchups: " (str/join ", " (map name bad))
                             "\navailable: " (str/join ", " (sort (map name all-matchups))))
                        {})))
      ks)))

(defn- quiet-sente-logging!
  "Sente 1.20+ logs :info per connection event via trove; with hundreds of
  clients that buries the status lines. Our own teardown disconnects arrive
  as warns, so drop exactly those too. Older sente has no trove and logs
  via timbre, quiet enough as-is."
  []
  (when-let [log-fn-var (try (requiring-resolve 'taoensso.trove/*log-fn*)
                             (catch java.io.FileNotFoundException _ nil))]
    (let [get-log-fn (requiring-resolve 'taoensso.trove.console/get-log-fn)
          console-log (get-log-fn {:min-level :warn})]
      ;; trove/set-log-fn! is a macro, so set its var like it would
      (alter-var-root log-fn-var
                      (constantly
                       (fn [ns coords level id lazy_]
                         (when-not (and (= id :sente.client/chsk-closed)
                                        (= :requested-disconnect (:reason (:data (force lazy_)))))
                           (console-log ns coords level id lazy_))))))))

(defn command
  "Entry point for `lein stress-test` / `bin/stress-test`."
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (do (println "Run concurrent bot games against a running server and sample its resource usage.")
                          (println)
                          (println summary))
      errors (do (doseq [e errors] (println e)) (System/exit 1))
      :else
      (let [_ (quiet-sente-logging!)
            {:keys [concurrent-games duration-seconds delay spectators url container pid out
                    save-replays profile nrepl-port chat-chance max-blocks lobby-watchers]} options
            matchups (try (resolve-matchups (:matchups options))
                          (catch Exception e
                            (println (.getMessage e))
                            (System/exit 1)))
            out-dir (io/file (or out (str "stress-runs/" (.truncatedTo (Instant/now) ChronoUnit/SECONDS))))
            counters (atom {:actions 0 :diffs 0 :errors 0 :latencies [] :chats 0
                            :lobby-lists 0 :games-completed 0 :wedges 0 :slot-resets 0})
            active-games (atom 0)
            stop? (atom false)
            latest-resources (atom nil)
            config (-> options (assoc :matchups (mapv name matchups)) (dissoc :help))
            ctx {:base-url url :counters counters :active-games active-games
                 :stop? stop? :delay-ms delay :spectators-per-game spectators
                 :save-replays? save-replays :chat-chance chat-chance
                 :games concurrent-games :max-blocks max-blocks
                 :wedge-timeout-ms (max 30000 (* 30 delay)) :matchups matchups}
            nrepl-opts {:host (.getHost (java.net.URI. url)) :port nrepl-port}
            profile-event (atom nil)
            ;; :ramp while logins/creates assemble, :steady once games are up,
            ;; nil once collected; guarded by profile-lock
            profile-phase (atom nil)
            profile-lock (Object.)
            docker-proc (when-not pid
                          (try
                            (docker-sampler! container latest-resources stop?)
                            (catch Exception e
                              (println "warning: resource sampling disabled:" (.getMessage e))
                              nil)))
            _ (println (format "Starting %d games (%d matchups, delay %dms, %d spectators/game) against %s for %ds"
                               concurrent-games (count matchups) delay spectators url duration-seconds))
            ctx (assoc ctx :auths (pre-authenticate! ctx lobby-watchers))
            _ (when profile
                (try
                  (reset! profile-event (profiler/start! nrepl-opts (:profile-event options)))
                  (reset! profile-phase :ramp)
                  (println "profiling server ramp-up with event" @profile-event)
                  (catch Exception e
                    (println "warning: profiling disabled:" (.getMessage e)))))
            watchers (if (pos? lobby-watchers)
                       (connect-watchers! ctx lobby-watchers)
                       [])
            threads (mapv (fn [id]
                            (doto (Thread. #(slot-loop ctx id) (str "stress-slot-" id))
                              (.start)))
                          (range concurrent-games))
            t0 (now-ms)
            deadline (+ t0 (* 1000 duration-seconds))
            collect! (fn [prefix]
                       (try
                         (profiler/stop-and-collect! nrepl-opts (when-not pid container)
                                                     out-dir @profile-event prefix)
                         (catch Exception e
                           (println "warning: profile collection failed:" (.getMessage e)))))]
        (when @profile-phase
          ;; the ramp and the loaded steady state have very different profiles;
          ;; collect them separately, switching once all games are up
          (doto (Thread.
                 (fn []
                   (client/wait-for #(or @stop? (>= @active-games concurrent-games))
                                    (max 90000 (* 1500 concurrent-games)))
                   (locking profile-lock
                     (when (and (= :ramp @profile-phase) (not @stop?))
                       (collect! "profile-ramp")
                       (try
                         (profiler/start! nrepl-opts (:profile-event options))
                         (reset! profile-phase :steady)
                         (println "ramp-up profile collected; profiling steady state")
                         (catch Exception e
                           (reset! profile-phase nil)
                           (println "warning: steady profiling failed to start:" (.getMessage e)))))))
                 "stress-profiler")
            (.setDaemon true)
            (.start)))
        (.mkdirs out-dir)
        (spit (io/file out-dir "config.edn") (pr-str config))
        (with-open [csv (io/writer (io/file out-dir "samples.csv"))]
          (.write csv (str (str/join "," (map name csv-columns)) "\n"))
          (loop [prev @counters
                 rows []
                 all-lats []]
            (Thread/sleep 1000)
            (when pid (reset! latest-resources (ps-sample pid)))
            (let [[snapshot _] (swap-vals! counters assoc :latencies [])
                  lats (:latencies snapshot)
                  row (sample-row t0 latest-resources snapshot prev active-games lats)]
              (.write csv (str (str/join "," (map #(fmt (row %)) csv-columns)) "\n"))
              (.flush csv)
              (when (and (= 5 (count rows)) (nil? @latest-resources))
                (println "warning: no resource samples yet; check --container or --pid"))
              (when (zero? (mod (count rows) 10))
                (println (status-line row)))
              (if (< (now-ms) deadline)
                (recur snapshot (conj rows row) (into all-lats lats))
                (do
                  (locking profile-lock
                    (when-let [phase @profile-phase]
                      (collect! (if (= :steady phase) "profile-steady" "profile-ramp"))
                      (reset! profile-phase nil)))
                  (println "Stopping...")
                  (reset! stop? true)
                  (doseq [t threads] (.join t 15000))
                  (doseq [w watchers]
                    (try (client/disconnect! w) (catch Exception _)))
                  (when docker-proc (.destroy docker-proc))
                  (let [summary (summarize (conj rows row) (into all-lats lats) config)]
                    (spit (io/file out-dir "summary.edn") (pr-str summary))
                    (println)
                    (println "Summary:")
                    (println (json/generate-string summary {:pretty true}))
                    (println)
                    (println "Results in" (str out-dir))))))))
        (System/exit 0)))))
