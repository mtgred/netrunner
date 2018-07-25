(in-ns 'game.cards.ice)

(def card-definition-mausolus
  {"Mausolus"
   {:advanceable :always
    :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                   :msg (msg "gain " (if (wonder-sub card 3) 3 1) "[Credits]")
                   :effect (effect (gain-credits (if (wonder-sub card 3) 3 1)))}
                  {:label "Do 1 net damage (Do 3 net damage)"
                   :async true
                   :msg (msg "do " (if (wonder-sub card 3) 3 1) " net damage")
                   :effect (effect (damage eid :net (if (wonder-sub card 3) 3 1) {:card card}))}
                  {:label "Give the Runner 1 tag (and end the run)"
                   :async true
                   :msg (msg "give the Runner 1 tag"
                             (when (wonder-sub card 3)
                               " and end the run"))
                   :effect (req (gain-tags state :corp eid 1)
                                (when (wonder-sub card 3)
                                  (end-run state side)))}]}})
