(in-ns 'game.core)

(def card-definitions-assets-aryabhata-tech
  {"Aryabhata Tech"
   {:events {:successful-trace {:msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
                                :effect (effect (gain :credit 1)
                                                (lose :runner :credit 1))}}}})
