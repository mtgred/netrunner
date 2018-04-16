(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-nisei-division-the-next-generation
  {"Nisei Division: The Next Generation"
   {:events {:psi-game {:msg "gain 1 [Credits]" :effect (effect (gain :corp :credit 1))}}}})