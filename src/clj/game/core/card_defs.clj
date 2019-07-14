(ns game.core.card-defs
  (:require [clojure.string :refer [starts-with? ends-with?]]
            [clojure.java.io :refer [file]]
            [clojure.stacktrace :refer [print-stack-trace]]))

(defn- load-all-cards
  "Load all card definitions into their own namespaces"
  ([] (load-all-cards nil))
  ([path]
   (doall (pmap load-file
                (->> (file (str "src/clj/game/cards" (when path (str "/" path ".clj"))))
                     (file-seq)
                     (filter #(and (.isFile %)
                                   (ends-with? % ".clj")))
                     (map str))))))

(defn- get-card-defs
  ([] (get-card-defs nil))
  ([path]
   (->> (all-ns)
        (filter #(starts-with? % (str "game.cards" (when path (str "." path)))))
        (map #(ns-resolve % 'card-definitions))
        (map var-get)
        (apply merge))))

(def card-defs {})

(defn reset-card-defs
  "Performs any once only initialization that should be performed on startup"
  ([] (reset-card-defs nil))
  ([path]
   (let [cards-var #'game.core.card-defs/card-defs]
     (alter-var-root cards-var
                     (constantly
                       (merge card-defs
                              (do (load-all-cards path)
                                  (get-card-defs path))))))
   'loaded))

(defn card-def
  "Retrieves a card's abilities definition map."
  [card]
  (if-let [title (:title card)]
    (get card-defs title)
    (.println *err* (with-out-str
                      (print-stack-trace
                        (Exception. (str "Tried to select card def for non-existent card: " card))
                        25)))))
