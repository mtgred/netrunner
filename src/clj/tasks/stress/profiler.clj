(ns tasks.stress.profiler
  "Drives clj-async-profiler inside the server JVM over nREPL and collects the
  results into the stress run's output directory: collapsed stacks (text, one
  'frame;frame;... count' line per unique stack), a flamegraph SVG, and a
  top-frames text summary."
  (:require
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.string :as str]
    [nrepl.core :as nrepl]))

(defn- eval-remote
  "Evaluates code in the server JVM. Returns the printed value; throws on error."
  [{:keys [host port]} code]
  (with-open [conn (nrepl/connect :host host :port port)]
    (let [responses (doall (nrepl/message (nrepl/client conn 120000) {:op "eval" :code code}))
          err (some :err responses)
          value (first (keep :value responses))]
      (when err
        (throw (ex-info (str/trim err) {:code code})))
      value)))

(defn start!
  "Starts profiling in the server JVM. For cpu, falls back to itimer where
  perf events are unavailable (typical inside docker). Returns the event used."
  [nrepl-opts preferred-event]
  (eval-remote nrepl-opts "(require 'clj-async-profiler.core)")
  (let [candidates (if (= preferred-event "cpu")
                     [":cpu" ":itimer"]
                     [(str ":" preferred-event)])]
    (or (some (fn [event]
                (try
                  ;; big framebuf: deep clojure stacks overflow the 1MB default
                  (eval-remote nrepl-opts (format "(clj-async-profiler.core/start {:event %s :framebuf 10000000})" event))
                  event
                  (catch Exception _ nil)))
              candidates)
        (throw (ex-info (str "profiler failed to start with " (str/join ", " candidates)) {})))))

(defn- collect-collapsed!
  "Stops the profiler and brings the collapsed stacks file into out-dir."
  [nrepl-opts container out-file]
  (let [remote-path (read-string
                     (eval-remote nrepl-opts
                                  "(.getAbsolutePath (clj-async-profiler.core/stop {:generate-flamegraph? false}))"))]
    (if container
      ;; absolute destination: docker cp reads a relative path with a colon
      ;; (like our timestamped run folders) as container:path
      (let [{:keys [exit err]} (shell/sh "docker" "cp" (str container ":" remote-path)
                                         (.getAbsolutePath (io/file out-file)))]
        (when-not (zero? exit)
          (throw (ex-info (str "docker cp failed: " err) {}))))
      (io/copy (io/file remote-path) out-file))))

(defn- parse-collapsed [collapsed-file]
  (with-open [rdr (io/reader collapsed-file)]
    (doall
     (keep (fn [line]
             (let [sep (str/last-index-of line " ")]
               (when sep
                 [(str/split (subs line 0 sep) #";")
                  (parse-long (subs line (inc sep)))])))
           (line-seq rdr)))))

(defn- top-frames
  "Sample counts per frame: :self for leaf time, :total for time anywhere on
  the stack (each frame counted once per stack to keep recursion honest)."
  [stacks]
  (let [totals (reduce (fn [acc [frames n]]
                         (reduce #(update %1 %2 (fnil + 0) n) acc (distinct frames)))
                       {} stacks)
        selfs (reduce (fn [acc [frames n]]
                        (update acc (peek frames) (fnil + 0) n))
                      {} stacks)]
    {:total-samples (reduce + 0 (map second stacks))
     :by-total (sort-by val > totals)
     :by-self (sort-by val > selfs)}))

(defn- write-summary! [collapsed-file summary-file event]
  (let [{:keys [total-samples by-total by-self]} (top-frames (parse-collapsed collapsed-file))
        pct #(format "%5.1f%%" (double (* 100 (/ % total-samples))))
        section (fn [title entries]
                  (into [title]
                        (map (fn [[frame n]] (str (pct n) " " n " " frame))
                             (take 40 entries))))]
    (->> (concat
          [(str "Server profile, event " event ", " total-samples " samples")
           "Full stacks in the sibling -collapsed.txt (frame;frame;... count), flamegraph in the sibling .html"
           ""]
          (section "== Top frames by self time (time spent in the frame itself) ==" by-self)
          [""]
          (section "== Top frames by total time (frame plus everything it calls) ==" by-total))
         (str/join "\n")
         (spit summary-file))))

(defn- write-flamegraph! [collapsed-file html-file]
  (let [generate (requiring-resolve 'clj-async-profiler.core/generate-flamegraph)
        rendered (generate (str collapsed-file) {:title "netrunner stress run"})]
    (io/copy rendered (io/file html-file))))

(defn stop-and-collect!
  "Stops profiling and writes <prefix>-collapsed.txt, <prefix>-summary.txt and
  <prefix>.html into out-dir. Returns nil; failures print a warning and keep
  whatever was collected so far."
  [nrepl-opts container out-dir event prefix]
  (let [collapsed (io/file out-dir (str prefix "-collapsed.txt"))]
    (collect-collapsed! nrepl-opts container collapsed)
    (write-summary! collapsed (io/file out-dir (str prefix "-summary.txt")) event)
    (try
      (write-flamegraph! collapsed (io/file out-dir (str prefix ".html")))
      (catch Exception e
        (println "warning: flamegraph generation failed:" (.getMessage e))
        (println (str "         " prefix "-collapsed.txt still works with https://www.speedscope.app"))))))
