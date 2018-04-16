(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-haas-bioroid-engineering-the-future
  {"Haas-Bioroid: Engineering the Future"
   {:events {:corp-install {:req (req (first-event? state corp :corp-install))
                            :msg "gain 1 [Credits]"
                            :effect (effect (gain :credit 1))}}}})
