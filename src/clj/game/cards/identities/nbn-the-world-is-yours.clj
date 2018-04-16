(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-nbn-the-world-is-yours
  {"NBN: The World is Yours*"
   {:effect (effect (gain :hand-size-modification 1))
    :leave-play (effect (lose :hand-size-modification 1))}})