(in-ns 'game.core)

(def card-definitions-identities-haas-bioroid-engineering-the-future
  {"Haas-Bioroid: Engineering the Future"
   {:events {:corp-install {:req (req (first-event? state corp :corp-install))
                            :msg "gain 1 [Credits]"
                            :effect (effect (gain :credit 1))}}}})
