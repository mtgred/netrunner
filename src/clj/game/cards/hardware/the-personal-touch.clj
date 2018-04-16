(in-ns 'game.core)

(def card-hardware-the-personal-touch
  {"The Personal Touch"
   {:hosting {:req #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}})
