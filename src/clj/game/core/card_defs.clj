(ns game.core.card-defs)

(defmulti defcard-impl (fn [title] title))
(defmethod defcard-impl :default [_] nil)

(defn card-def
  "Retrieves a card's abilities definition map."
  [card]
  (if-let [title (:title card)]
    (or (defcard-impl title) {})
    (throw (ex-info "Tried to select card def for non-existent card" {:msg "Tried to select card-def for non existent card"
                                                                      :card card}))))
