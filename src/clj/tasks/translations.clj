(ns tasks.translations
  "Find missing translations"
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]
   [game.utils :refer [dissoc-in]]
   [i18n.core :as tr.core]
   [taoensso.encore :as encore])
  (:import
   [java.io File]))

(defn get-nodes [lang]
  (->> lang
       (get (tr.core/translation-dictionary))
       (encore/node-paths)
       (remove #(= :angel-arena (first %)))
       (into #{} (map (comp vec butlast)))))

(defn to-keyword [s]
  (cond
    (keyword? s) s
    (str/starts-with? s ":") (keyword (subs s 1))
    :else (keyword s)))

(defn missing-translations
  "Treat :en as the single source of truth. Compare each other language against it.
  Print when the other language is missing entries, and also print when the other
  language has defined entries not in :en.

  Ignores :angel-arena entries because that's being phased out."
  [& args]
  (let [en-nodes (get-nodes :en)
        other-langs (keys (dissoc (tr.core/translation-dictionary) :en))]
    (doseq [lang (or (seq (map to-keyword args)) other-langs)
            :let [lang-nodes (get-nodes lang)]]
      (println "Checking" lang)
      (when-let [diff (seq (set/difference en-nodes lang-nodes))]
        (println "Missing from" lang)
        (pp/pprint (sort diff))
        (newline))
      (when-let [diff (seq (set/difference lang-nodes en-nodes))]
        (println "Missing from :en")
        (pp/pprint (sort diff))
        (newline)))
    (println "Finished!")))

(comment
  (missing-translations))

(defn undefined-translations
  [& _args]
  (let [en-nodes (->> (get (tr.core/translation-dictionary) :en)
                      (encore/node-paths)
                      (map #(vector (vec (butlast %)) (last %)))
                      (into {}))
        files (->> (io/file "src")
                   (file-seq)
                   (filter #(.isFile ^File %))
                   (filter #(str/includes? (str %) ".clj"))
                   (map (juxt str slurp)))]
    (doseq [[file-name contents] files
            :let [used
                  (->> contents
                       (re-seq #"\(tr \[:(.*?)( \"(.*?)\")?\]")
                       (mapv (fn [[_ k _ default]]
                               [(mapv keyword (str/split k #"[/\.]"))
                                (when default
                                  (str/trim default))]))
                       (sort))]
            [k default] used
            :let [en-node (get en-nodes k)]
            :when (not (or (fn? en-node)
                           (keyword? en-node)))
            :when (if (and k default)
                    (not= (when en-node (str/lower-case en-node))
                          (str/lower-case default))
                    (not en-node))]
      (println file-name)
      (pp/pprint [k (when (or en-node default)
                      [en-node default])]))
    (println "Finished!")))

(comment
  (undefined-translations))

(def keys-to-dissoc
  #{:missing ; default text
    :card-type ; handled by tr-type
    :side ; handled by tr-side
    :faction ; handled by tr-faction
    :format ; handled by tr-format
    :lobby ; handled by tr-lobby and tr-watch-join
    :pronouns ; handled by tr-pronouns
    :set ; handled by tr-set
    :game-prompt ; handled by tr-game-prompt
    })

(def nested-keys-to-dissoc
  #{[:card-browser :sort-by] ; handled by tr-sort
    [:card-browser :influence] ; currently unused
    [:deck-builder :hash] ; currently unused
    })

(defn dissoc-programmatic-keys [dict]
  (reduce dissoc-in
          (apply dissoc dict keys-to-dissoc)
          nested-keys-to-dissoc))

(defn unused-translations
  [& _args]
  (let [regexen (->> (get (tr.core/translation-dictionary) :en)
                     (dissoc-programmatic-keys)
                     (encore/node-paths)
                     (map #(vec (butlast %)))
                     (map #(let [s (str/join "." (map name %))
                                 patt (str "\\(tr \\[(:" s ")( \\\"(.*?)\\\")?\\]")]
                             [% (re-pattern patt)]))
                     (into #{}))
        files (->> (io/file "src")
                   (file-seq)
                   (filter #(.isFile ^File %))
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
  (unused-translations))

;; should be a one-off
(defn convert-edn-to-fluent
  []
  (let [dict (tr.core/translation-dictionary)]
    (doseq [lang (keys dict)]
      (let [translation
            (with-out-str
              (doseq [path-ns (->> (get-nodes :en)
                                   (group-by first)
                                   (sort-by key))
                      ; slit each group by a newline
                      :let [_ (newline)]
                      path (->> path-ns
                                (val)
                                (sort))
                      :let [identifier (->> path
                                            (map #(str (symbol %)))
                                            (str/join "_"))
                            node (get-in dict (cons lang path))
                            node (if (string? node)
                                   (-> node
                                       (str/replace "{" "{\"{\"RRR")
                                       (str/replace "}" "{\"}\"}")
                                       (str/replace "RRR" "}"))
                                   node)]]
                (printf "%s = %s\n" identifier node)))]
        (spit (io/file "resources" "public" "i18n" (str (symbol lang) ".ftl"))
          (str/triml translation))))))

(comment
  (convert-edn-to-fluent))
