(in-ns 'game.cards.agendas)

(def card-definition-improved-tracers
  {"Improved Tracers"
   {:silent (req true)
    :effect (req (update-all-ice state side))
    :swapped {:effect (req (update-all-ice state side))}
    :events {:pre-ice-strength {:req (req (has-subtype? target "Tracer"))
                                :effect (effect (ice-strength-bonus 1 target))}
             :pre-init-trace {:req (req (and (has-subtype? target "Tracer")
                                             (= :subroutine (:source-type (second targets)))))
                              :effect (effect (init-trace-bonus 1))}}}})
