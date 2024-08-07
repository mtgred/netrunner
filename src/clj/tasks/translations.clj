(ns tasks.translations
  "Find missing translations"
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]
   [taoensso.encore :as encore]
   [translations.core :as tr.core]) 
  (:import
   [java.io File]))

(defn get-nodes [lang]
  (into #{} (map (comp vec butlast))
        (encore/node-paths
         (get tr.core/translation-dictionary lang))))

(defn to-keyword [s]
  (cond
    (keyword? s) s
    (str/starts-with? s ":") (keyword (subs s 1))
    :else (keyword s)))

(defn missing-translations
  [& args]
  (let [en-nodes (get-nodes :en)
        other-langs (keys (dissoc tr.core/translation-dictionary :en))]
    (doseq [lang (or (seq (map to-keyword args)) other-langs)
            :let [lang-nodes (get-nodes lang)]]
      (println "Checking" lang)
      (pp/pprint
       (sort (set/difference en-nodes lang-nodes)))
      (newline))))

(comment
  (missing-translations))

(defn undefined-translations
  [& _args]
  (let [en-nodes (->> (get tr.core/translation-dictionary :en)
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
                      [en-node default])]))))

(comment
  (undefined-translations))
