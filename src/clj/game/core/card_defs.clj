(ns game.core.card-defs
  (:require [clojure.string :refer [starts-with? ends-with?]]
            [clojure.java.io :refer [file]]
            [clojure.stacktrace :refer [print-stack-trace]]))

(def card-defs (atom {}))

(defn card-def
  "Retrieves a card's abilities definition map."
  [card]
  (if-let [title (:title card)]
    (get @card-defs title)
    (.println *err* (with-out-str
                      (print-stack-trace
                        (Exception. (str "Tried to select card def for non-existent card: " card))
                        2500)))))

(defn define-card
  [title ability]
  (swap! card-defs assoc title ability)
  ability)
