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
    Attribute
    CallArguments
    Commentary$Comment
    Commentary$GroupComment
    Commentary$ResourceComment
    Identifiable
    Identifier
    InlineExpression$FunctionReference
    InlineExpression$MessageReference
    InlineExpression$TermReference
    InlineExpression$VariableReference
    Literal$StringLiteral
    Message
    NamedArgument
    Pattern
    PatternElement$Placeable
    PatternElement$TextElement
    SelectExpression
    Term
    Variant)
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

(let [langs (->> (io/file "resources/public/i18n")
                 (file-seq)
                 (filter #(.isFile ^java.io.File %))
                 (filter #(str/ends-with? (str %) ".ftl"))
                 (map (fn [^java.io.File f]
                        (let [n (str/replace (.getName f) ".ftl" "")
                              content (slurp f)]
                          [n content]))))]
  (reset! fluent-dictionary {})
  (doseq [[lang content] langs]
    (swap! fluent-dictionary assoc lang (build lang content)))
  (println "Loaded"))

(defn get-messages [lang]
  (->> (get @fluent-dictionary lang)
       (remove #(str/starts-with? "angel-arena" (first %)))
       (into {})))

(defn to-string [s]
  (if (string? s) s (name s)))

(defn missing-translations
  "Treat :en as the single source of truth. Compare each other language against it.
  Print when the other language is missing entries, and also print when the other
  language has defined entries not in :en."
  [& args]
  (let [en-keys (keys (get-messages "en"))]
    (doseq [lang (or (seq (map to-string args))
                     (keys (dissoc @fluent-dictionary "en")))
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

;; formatting

(defn- indent [ctx]
  (print (str/join (repeat (:indent ctx) "    "))))

(defprotocol FTLPrinter
  (-ftl-print [this ctx]))

(extend-protocol FTLPrinter
  Commentary$Comment
  (-ftl-print
   [this _ctx]
   (print "# ")
   (print (Commentary$Comment/.text this)))

  Commentary$GroupComment
  (-ftl-print
   [this _ctx]
   (newline)
   (print "## ")
   (print (Commentary$GroupComment/.text this)))

  Commentary$ResourceComment
  (-ftl-print
   [this _ctx]
   (newline)
   (print "### ")
   (print (Commentary$ResourceComment/.text this)))

  Literal$StringLiteral
  (-ftl-print
   [this _ctx]
   (pr (Literal$StringLiteral/.value this)))

  Identifier
  (-ftl-print
   [this _ctx]
   (print (Identifier/.key this)))

  InlineExpression$MessageReference
  (-ftl-print
   [this ctx]
   (-ftl-print (InlineExpression$MessageReference/.identifier this) ctx))

  InlineExpression$TermReference
  (-ftl-print
   [this ctx]
   (print "{-")
   (-ftl-print (InlineExpression$TermReference/.identifier this) ctx)
   (when-let [attr (Optional/.orElse (InlineExpression$TermReference/.attributeID this) nil)]
     (-ftl-print attr ctx))
   (when-let [args (Optional/.orElse (InlineExpression$TermReference/.arguments this) nil)]
     (let [positional (not-empty (CallArguments/.positional args))
           named (not-empty (CallArguments/.named args))]
       (print "(")
       (let [ctx (assoc ctx :arg true)]
         (when positional
           (run! (fn [arg] (if (string? arg)
                             (print arg)
                             (-ftl-print arg ctx)))
                 (interpose ", " positional)))
         (when (and positional named)
           (print ", "))
         (when named
           (run! (fn [arg] (if (string? arg)
                             (print arg)
                             (-ftl-print arg ctx)))
                 (interpose ", " named))))
       (print ")")))
   (print "}"))

  InlineExpression$FunctionReference
  (-ftl-print
   [this ctx]
   (-ftl-print (InlineExpression$FunctionReference/.identifier this) ctx)
   (when-let [args (Optional/.orElse (InlineExpression$FunctionReference/.arguments this) nil)]
     (let [positional (not-empty (CallArguments/.positional args))
           named (not-empty (CallArguments/.named args))]
       (print "(")
       (let [ctx (assoc ctx :arg true)]
         (when positional
           (run! (fn [arg] (if (string? arg)
                             (print arg)
                             (-ftl-print arg ctx)))
                 (interpose ", " positional)))
         (when (and positional named)
           (print ", "))
         (when named
           (run! (fn [arg] (if (string? arg)
                             (print arg)
                             (-ftl-print arg ctx)))
                 (interpose ", " named))))
       (print ")"))))

  NamedArgument
  (-ftl-print
   [this ctx]
   (-ftl-print (NamedArgument/.name this) ctx)
   (print ": ")
   (-ftl-print (NamedArgument/.value this) ctx))

  InlineExpression$VariableReference
  (-ftl-print
   [this ctx]
   (print "$")
   (-ftl-print (InlineExpression$VariableReference/.identifier this) ctx))

  Variant
  (-ftl-print
   [this ctx]
   (when (Variant/.isDefault this)
     (print "*"))
   (print "[")
   (-ftl-print (Variant/.keyable this) ctx)
   (print "]")
   (print " ")
   (-ftl-print (Variant/.value this) ctx))

  SelectExpression
  (-ftl-print
   [this ctx]
   (-ftl-print (SelectExpression/.selector this) ctx)
   (print " ->")
   (newline)
   (run! (fn [variant]
           (indent ctx)
           (-ftl-print variant ctx)
           (newline))
         (SelectExpression/.variants this))
   (indent (update ctx :indent #(max 0 (dec %)))))

  PatternElement$TextElement
  (-ftl-print
   [this ctx]
   (let [s (PatternElement$TextElement/.value this)]
     (print s)
     (when (str/includes? s "\n")
       (indent ctx))))

  PatternElement$Placeable
  (-ftl-print
   [this ctx]
   (print "{")
   (-ftl-print (PatternElement$Placeable/.expression this) ctx)
   (print "}"))

  Pattern
  (-ftl-print
   [this ctx]
   (let [ctx (update ctx :indent inc)]
     (run! (fn [pat] (-ftl-print pat ctx)) (Pattern/.elements this))))

  Attribute
  (-ftl-print
   [this ctx]
   (let [ctx (update ctx :indent inc)]
     (indent ctx)
     (print ".")
     (-ftl-print (Attribute/.identifier this) ctx)
     (print " = ")
     (-ftl-print (Attribute/.pattern this) ctx)
     (newline)))

  Term
  (-ftl-print
   [this ctx]
   (Optional/.map (Term/.comment this) (fn [cmnt]
                                         (-ftl-print cmnt ctx)
                                         (newline)))
   (print "-")
   (-ftl-print (Term/.identifier this) ctx)
   (print " = ")
   (-ftl-print (Term/.value this) ctx)
   (when-let [attrs (not-empty (Term/.attributes this))]
     (newline)
     (run! (fn [attr] (-ftl-print attr ctx)) attrs))
   (newline))

  Message
  (-ftl-print
   [this ctx]
   (Optional/.map (Message/.comment this) (fn [cmnt]
                                            (-ftl-print cmnt ctx)
                                            (newline)))
   (-ftl-print (Message/.identifier this) ctx)
   (print " = ")
   (-ftl-print (Optional/.orElseThrow (Message/.pattern this)) ctx)
   (newline))

  FluentResource
  (-ftl-print
   [this ctx]
   (let [entries (FluentResource/.entries this)]
     (doseq [entry entries]
       (-ftl-print entry ctx)
       (newline)))))

(comment
  (let [res (FTLParser/parse (FTLStream/of "a = {NUMBER($percent, style:\"percent\")}") false)]
    (prn res)
    (-ftl-print res {:indent 0})))

(defn ftl-print
  ([resource]
   (ftl-print resource {:indent 0}))
  ([resource config]
   (-ftl-print resource config)))

(defn format-i18n-files
  [& args]
  (let [langs (or (seq (map to-string args))
                  (keys @fluent-dictionary))]
    (doseq [lang langs
            :let [f (io/file "resources" "public" "i18n" (str lang ".ftl"))
                  contents (-> (slurp f)
                               (str/replace "\\u" "__u")
                               (str/replace "\\U" "__U"))
                  ast (FTLParser/parse (FTLStream/of contents) false)
                  formatted-file (-> (with-out-str (ftl-print ast))
                                     (str/replace "__u" "\\u")
                                     (str/replace "__U" "\\U")
                                     (str/trim)
                                     (str "\n"))]]
      (spit f formatted-file))))

(comment
  (format-i18n-files))
