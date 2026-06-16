(ns tasks.translations
  "Find missing translations"
  (:require
   [clojure.java.io :as io]
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
   (java.io File)
   (java.util Locale Optional)))

(set! *warn-on-reflection* true)

(def fluent-dictionary
  (atom {}))

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


(let [langs (keep (fn [lang]
                    (let [ui (io/file "resources" "public" "i18n" lang "ui.ftl")]
                      (when (.exists ui)
                        (let [content (slurp ui)]
                          (when-not (str/blank? content)
                            [lang content])))))
                  ["ca" "en" "es" "fr" "it" "ja" "ko" "la-pig" "pl" "pt" "ru" "zh-simp" "zh-trad"])]
  (reset! fluent-dictionary {})
  (doseq [[lang content] langs]
    (swap! fluent-dictionary assoc lang (build lang content)))
  (println "Loaded"))

(defn get-messages [lang]
  (->> (get @fluent-dictionary lang)
       (remove #(str/starts-with? "angel-arena" (first %)))
       (into {})))

(defn missing-translations
  "Treat :en as the single source of truth. Compare each other language against it.
  Print when the other language is missing entries, and also print when the other
  language has defined entries not in :en."
  [& args]
  (let [en-keys (keys (get-messages "en"))
        langs (or (seq (map name args))
                  (keys (dissoc @fluent-dictionary "en")))]
    (doseq [lang (sort langs)
            :let [lang-keys (keys (get-messages lang))]]
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
  (missing-translations))

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

(defn undefined-translations
  [& _args]
  (let [en-map (get-messages "en")
        files (->> (concat (file-seq (io/file "src/cljs"))
                           (file-seq (io/file "src/cljc")))
                   (filter #(.isFile ^File %))
                   (filter #(str/includes? (str %) ".clj"))
                   (remove #(str/includes? (str %) "angel_arena"))
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
  (undefined-translations))

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

(defn unused-translations
  [& _args]
  (let [regexen (->> (get-messages "en")
                     (keys)
                     (remove (fn [k]
                               (some #(str/starts-with? k %)
                                     keys-to-dissoc)))
                     (map #(let [patt (str "tr \\[:" % "(( \\\"(.*?)\\\")| [a-zA-Z0-9_-]*?)?\\]")]
                             [% (re-pattern patt)]))
                     (into #{}))
        files (->> (concat (file-seq (io/file "src/cljs"))
                           (file-seq (io/file "src/cljc")))
                   (filter #(.isFile ^File %))
                   (filter #(str/includes? (str %) ".clj"))
                   (remove #(str/includes? (str %) "angel_arena"))
                   (map (juxt str slurp)))]
    (doseq [[path regex] (sort regexen)
            :when (->> files
                       (filter (fn [[_file-name contents]]
                                 (seq (re-seq regex contents))))
                       (empty?))]
      (println path))
    (println "Finished!")))

(comment
  (unused-translations))
