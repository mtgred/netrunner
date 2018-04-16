(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-improved-tracers
  {"Improved Tracers"
   {:silent (req true)
    :effect (req (update-all-ice state side))
    :swapped {:effect (req (update-all-ice state side))}
    :events {:pre-ice-strength {:req (req (has-subtype? target "Tracer"))
                                :effect (effect (ice-strength-bonus 1 target))}
             :pre-init-trace {:req (req (has-subtype? target "Tracer"))
                              :effect (effect (init-trace-bonus 1))}}}})