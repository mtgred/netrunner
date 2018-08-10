(in-ns 'game.cards.identities)

(def card-definition-haas-bioroid-engineering-the-future
  {"Haas-Bioroid: Engineering the Future"
   {:events {:corp-install {:req (req (first-event? state corp :corp-install))
                            :msg "gain 1 [Credits]"
                            :effect (effect (gain-credits 1))}}}})
