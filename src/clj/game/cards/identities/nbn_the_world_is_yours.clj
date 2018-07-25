(in-ns 'game.cards.identities)

(def card-definition-nbn-the-world-is-yours
  {"NBN: The World is Yours*"
   {:effect (effect (gain :hand-size 1))
    :leave-play (effect (lose :hand-size 1))}})
