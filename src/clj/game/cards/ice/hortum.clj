(in-ns 'game.cards.ice)

(def card-definition-hortum
  {"Hortum"
   (letfn [(hort [n] {:prompt "Choose a card to add to HQ with Hortum"
                      :async true
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
                                         (effect-completed state side eid))))})]
     {:advanceable :always
      :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                     :msg (msg "gain " (if (wonder-sub card 3) "4" "1") " [Credits]")
                     :effect (effect (gain-credits :corp (if (wonder-sub card 3) 4 1)))}
                    {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                     :async true
                     :effect (req (if (wonder-sub card 3)
                                    (wait-for
                                      (resolve-ability state side (hort 1) card nil)
                                      (do (end-run state side)
                                          (system-msg state side
                                                      (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                           "shuffle R&D, and end the run"))))
                                    (do (end-run state side)
                                        (system-msg state side (str "uses Hortum to end the run"))
                                        (effect-completed state side eid))))}]})})
