(ns game.core.card-defs
  (:require [clojure.stacktrace :refer [print-stack-trace]]))

(defmulti defcard-impl (fn [title] title))
(defmethod defcard-impl :default [_] nil)

(defn card-def
  "Retrieves a card's abilities definition map."
  [card]
  (if-let [title (:title card)]
    (or (defcard-impl title) {})
    (.println *err* (with-out-str
                      (print-stack-trace
                        (Exception. (str "Tried to select card def for non-existent card: " card))
                        2500)))))
