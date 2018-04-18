(in-ns 'game.core)

(def card-definitions-ice-hortum
  {"Hortum"
   (letfn [(hort [n] {:prompt "Choose a card to add to HQ with Hortum"
                      :delayed-completion true
                      :choices (req (cancellable (:deck corp) :sorted))
                      :msg "add 1 card to HQ from R&D"
                      :cancel-effect (req (shuffle! state side :deck)
                                          (system-msg state side (str "shuffles R&D"))
                                          (effect-completed state side eid))
                      :effect (req (move state side target :hand)
                                   (if (< n 2)
                                     (continue-ability state side (hort (inc n)) card nil)
                                     (do (shuffle! state side :deck)
                                         (system-msg state side (str "shuffles R&D"))
                                         (effect-completed state side eid card))))})]
     {:advanceable :always
      :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                     :msg (msg "gain " (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2) "4" "1") " [Credits]")
                     :effect (effect (gain :corp :credit (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2) 4 1)))}
                    {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                     :delayed-completion true
                     :effect (req (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2)
                                    (when-completed (resolve-ability state side (hort 1) card nil)
                                                    (do (end-run state side)
                                                        (system-msg state side (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                                                    "shuffle R&D, and end the run"))))
                                    (do (end-run state side)
                                        (system-msg state side (str "uses Hortum to end the run"))
                                        (effect-completed state side eid))))}]})})
