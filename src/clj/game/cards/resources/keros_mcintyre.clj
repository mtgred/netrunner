(in-ns 'game.cards.resources)

(def card-definition-keros-mcintyre
  {"Keros Mcintyre"
   {:events
    {:derez
     {:req (req (and (first-event? state side :derez)
                     (= (second targets) :runner)))
      :once :per-turn
      :msg "gain 2 [Credits]"
      :effect (effect (gain-credits 2))}}}})
