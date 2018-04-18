(in-ns 'game.core)

(def card-definitions-identities-armand-geist-walker-tech-lord
  {"Armand \"Geist\" Walker: Tech Lord"
   {:events {:runner-trash {:req (req (and (= side :runner) (= (second targets) :ability-cost)))
                            :msg "draw a card"
                            :effect (effect (draw 1))}}}})
