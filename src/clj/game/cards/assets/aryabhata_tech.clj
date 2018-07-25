(in-ns 'game.cards.assets)

(def card-definition-aryabhata-tech
  {"Aryabhata Tech"
   {:events {:successful-trace {:msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
                                :effect (effect (gain-credits 1)
                                                (lose-credits :runner 1))}}}})
