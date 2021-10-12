(ns game.core.card-defs)

(defmulti defcard-impl (fn [title] title))
(defmethod defcard-impl :default [_] nil)

(defn card-def
  "Retrieves a card's abilities definition map."
  [{:keys [printed-title title] :as card}]
  (cond
    title (or (defcard-impl title) {})
    printed-title (or (defcard-impl printed-title) {})
    :else (throw (ex-info "Tried to select card def for non-existent card"
                          {:msg "Tried to select card-def for non existent card"
                           :card card}))))
