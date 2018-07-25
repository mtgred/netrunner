(in-ns 'game.cards.assets)

(def card-definition-rashida-jaheem
  {"Rashida Jaheem"
   (let [ability {:once :per-turn
                  :label "Gain 3 [Credits] and draw 3 cards (start of turn)"
                  :effect (effect (resolve-ability
                                    {:optional
                                     {:prompt "Trash Rashida Jaheem to gain 3 [Credits] and draw 3 cards?"
                                      :yes-ability {:async true
                                                    :msg "gain 3 [Credits] and draw 3 cards"
                                                    :effect (req (wait-for (trash state side card nil)
                                                                           (do (gain-credits state side 3)
                                                                               (draw state side eid 3 nil))))}}}
                                    card nil))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})})
