(in-ns 'game.core)

(def card-definitions-identities-cybernetics-division-humanity-upgraded
  {"Cybernetics Division: Humanity Upgraded"
   {:effect (effect (lose :hand-size-modification 1)
                    (lose :runner :hand-size-modification 1))
    :leave-play (effect (gain :hand-size-modification 1)
                        (gain :runner :hand-size-modification 1))}})
