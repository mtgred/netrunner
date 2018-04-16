(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-chaos-theory-wunderkind
  {"Chaos Theory: Wünderkind"
   {:effect (effect (gain :memory 1))
    :leave-play (effect (lose :runner :memory 1))}})
