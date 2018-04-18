(in-ns 'game.core)

(def card-definitions-ice-mausolus
  {"Mausolus"
   {:advanceable :always
    :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                   :msg (msg "gain " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " [Credits]")
                   :effect (effect (gain :credit (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3)))}
                  {:label "Do 1 net damage (Do 3 net damage)"
                   :delayed-completion true
                   :msg (msg "do " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " net damage")
                   :effect (effect (damage eid :net (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) {:card card}))}
                  {:label "Give the Runner 1 tag (and end the run)"
                   :delayed-completion true
                   :msg (msg "give the Runner 1 tag"
                             (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) " and end the run"))
                   :effect (req (tag-runner state :runner eid 1)
                                (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))
                                  (end-run state side)))}]}})
