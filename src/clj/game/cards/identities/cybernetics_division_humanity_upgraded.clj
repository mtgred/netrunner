(in-ns 'game.cards.identities)

(def card-definition-cybernetics-division-humanity-upgraded
  {"Cybernetics Division: Humanity Upgraded"
   {:effect (effect (lose :hand-size 1)
                    (lose :runner :hand-size 1))
    :leave-play (effect (gain :hand-size 1)
                        (gain :runner :hand-size 1))}})
