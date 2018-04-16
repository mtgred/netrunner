(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-aryabhata-tech
  {"Aryabhata Tech"
   {:events {:successful-trace {:msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
                                :effect (effect (gain :credit 1)
                                                (lose :runner :credit 1))}}}})
