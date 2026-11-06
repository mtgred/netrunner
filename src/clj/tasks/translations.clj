(ns tasks.translations
  "Find missing translations"
  (:require
   [babashka.fs :as fs]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str])
  (:import
   (fluent.bundle FluentBundle FluentBundle$Builder FluentResource)
   (fluent.functions.cldr CLDRFunctionFactory)
   (fluent.syntax.AST
    Identifiable
    Message
    Pattern
    PatternElement$TextElement)
   (fluent.syntax.parser FTLParser FTLStream)
   (java.util Locale Optional)))

(set! *warn-on-reflection* true)

(def fluent-dictionary (atom {}))

(defn get-id
  [entry]
  (when (instance? Identifiable entry)
    (.name ^Identifiable entry)))

(defn build
  [locale-str ^String resource]
  (try
    (let [locale-str (if (= "la-pig" locale-str) "en" locale-str)
          locale (Locale/forLanguageTag locale-str)
          builder (FluentBundle/builder locale CLDRFunctionFactory/INSTANCE)
          ftl-res (FTLParser/parse (FTLStream/of resource))
          entries (into {} (keep (fn [e] (when-let [id (get-id e)] [id e])))
                        (FluentResource/.entries ftl-res))]
      (when (FluentResource/.hasErrors ftl-res)
        (let [errors (.errors ftl-res)
              err (first errors)]
          (throw (ex-info (str "Error in " locale-str " adding resource: " (ex-message err))
                          {:locale locale-str
                           :errors (mapv ex-message errors)}
                          err))))
      (FluentBundle$Builder/.addResource builder ftl-res)
      (FluentBundle$Builder/.build builder)
      entries)
    (catch Throwable err
      (println (str "Error in " locale-str " adding resource: " (ex-message err)))
      (throw err))))

(defn load-dictionary!
  []
  (let [dir "resources/public/i18n"
        langs (->> ["ca" "de" "en" "es" "fr" "it" "ja" "ko" "la-pig" "pl" "pt" "ru" "zh-simp" "zh-trad"]
                   (keep (fn [lang]
                           (let [ui (let [f (fs/file dir lang "ui.ftl")]
                                      (when (fs/exists? f)
                                        (slurp f)))
                                 msgs (let [f (fs/file dir lang "messages.ftl")]
                                        (when (fs/exists? f)
                                          (slurp f)))]
                             (when (or ui msgs)
                               (cond-> [lang]
                                 (not (str/blank? ui)) (conj [:ui ui])
                                 (not (str/blank? msgs)) (conj [:messages msgs])))))))
        errors (volatile! [])]
    (reset! fluent-dictionary {})
    (doseq [[lang & contents] langs
            [kind content] contents
            :when content]
      (swap! fluent-dictionary assoc-in [lang kind] (build lang content)))
    @errors))

(defn get-messages
  ([lang] (into {} (mapcat val (get @fluent-dictionary lang))))
  ([lang kind] (into {} (get-in @fluent-dictionary [lang (keyword (name kind))]))))

(defn missing-translations
  "Treat :en as the single source of truth. Compare each other language against it.
  Print when the other language is missing entries, and also print when the other
  language has defined entries not in :en."
  [kind & args]
  (assert (#{"ui" "messages"} (name kind)) (str "Must be 'ui' or 'messages': " (pr-str kind)))
  (load-dictionary!)
  (let [en-keys (keys (get-messages "en" kind))
        langs (or (seq (map name args))
                  (keys (dissoc @fluent-dictionary "en")))]
    (doseq [lang (sort langs)
            :let [lang-keys (keys (get-messages lang kind))]]
      (println "Checking" lang)
      (when-let [diff (seq (set/difference
                            (->> en-keys
                                 (remove #(str/starts-with? % "preconstructed_"))
                                 (set))
                            (set lang-keys)))]
        (println "Missing from" lang)
        (pp/pprint (sort diff))
        (newline))
      (when-let [diff (seq (set/difference
                            (set lang-keys)
                            (set en-keys)))]
        (println "Missing from :en")
        (pp/pprint (sort diff))
        (newline)))
    (println "Finished!")))

(comment
  (missing-translations :messages))

(defn get-value
  [message]
  (when-let [elements (some-> message
                              (Message/.pattern)
                              (Optional/.orElse nil)
                              (Pattern/.elements)
                              (seq)
                              vec)]
    (when (every? #(instance? PatternElement$TextElement %) elements)
      (->> elements
           (map PatternElement$TextElement/.value)
           (str/join " ")))))

(defn undefined-clj-translations []
  (load-dictionary!)
  (let [en-map (get-messages "en" :messages)
        files (->> (concat (file-seq (fs/file "src/clj"))
                           (file-seq (fs/file "src/cljc")))
                   (filter fs/regular-file?)
                   (remove #(str/includes? (str %) ".cljs"))
                   (map (juxt str slurp)))
        finds
        (for [[file-name contents] files
              [kind k] (->> contents
                            (re-seq #"(?:(:msg/type|->msg-map)|(:effect/type)) :([a-zA-Z0-9_-]*)")
                            (keep (fn [[all msg? effect? msg-name]]
                                    (when-not (#{"as" "keyword"} msg-name)
                                      [(cond msg? :msg effect? :effect :else all) msg-name])))
                            (sort))
              :let [entry (get en-map (name k))
                    message (get-value entry)]
              :when (not entry)]
          {:file-name file-name
           :kind kind
           :entry k
           :msg message})
        grouped-finds (group-by :file-name finds)]
    (doseq [file-name (keys grouped-finds)
            {:keys [entry msg]} (get grouped-finds file-name)]
      (println file-name)
      (pp/pprint [entry msg]))
    (println "Finished!")))

(comment
  (undefined-clj-translations))

(defn undefined-cljs-translations []
  (load-dictionary!)
  (let [en-map (get-messages "en" :ui)
        files (->> (concat (file-seq (fs/file "src/cljs"))
                           (file-seq (fs/file "src/cljc")))
                   (filter fs/regular-file?)
                   (filter #(str/includes? (str %) ".clj"))
                   (map (juxt str slurp)))
        finds
        (for [[file-name contents] files
              :let [used
                    (->> contents
                         (re-seq #"tr \[:([a-zA-Z0-9_-]*?)( \"(.*?)\")?\]")
                         (mapv (fn [[_ k _ default]]
                                 [(keyword k)
                                  (when default
                                    (str/trim default))]))
                         (sort))]
              [k default] used
              :let [entry (get en-map (name k))
                    message (get-value entry)]
              :when (not entry)
              :when (if (and k default)
                      (not= (when message (str/lower-case message))
                            (str/lower-case default))
                      true)]
          {:file-name file-name
           :entry k
           :msg [message default]})
        grouped-finds (group-by :file-name finds)]
    (doseq [file-name (keys grouped-finds)
            {:keys [entry msg]} (get grouped-finds file-name)]
      (println file-name)
      (pp/pprint [entry msg]))
    (println "Finished!")))

(comment
  (undefined-cljs-translations))

(defn undefined-translations
  [kind]
  (if (#{"frontend" "cljs" "ui"} kind)
    (undefined-cljs-translations)
    (undefined-clj-translations)))

(def keys-to-dissoc
  #{"card-type_" ; handled by tr-type
    "side_" ; handled by tr-side
    "faction_" ; handled by tr-faction
    "format_" ; handled by tr-format
    "lobby_" ; handled by tr-lobby and tr-watch-join
    "pronouns" ; handled by tr-pronouns
    "set_" ; handled by tr-set
    "game_prompt" ; handled by tr-game-prompt
    "preconstructed_"
    })

(defn unused-cljs-translations
  [& _args]
  (load-dictionary!)
  (let [regexen (->> (get-messages "en" :ui)
                     (keys)
                     (remove (fn [k]
                               (some #(str/starts-with? k %)
                                     keys-to-dissoc)))
                     (map #(let [patt (str "tr \\[:" % "(( \\\"(.*?)\\\")| [a-zA-Z0-9_-]*?)?\\]")]
                             [% (re-pattern patt)]))
                     (into #{}))
        files (->> (concat (file-seq (fs/file "src/cljs"))
                           (file-seq (fs/file "src/cljc")))
                   (filter fs/regular-file?)
                   (filter #(str/includes? (str %) ".clj"))
                   (map (juxt str slurp)))]
    (doseq [[path regex] (sort regexen)
            :when (->> files
                       (filter (fn [[_file-name contents]]
                                 (seq (re-seq regex contents))))
                       (empty?))]
      (println path))
    (println "Finished!")))

(comment
  (unused-cljs-translations))
