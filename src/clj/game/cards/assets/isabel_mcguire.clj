(in-ns 'game.cards.assets)

(def card-definition-isabel-mcguire
  {"Isabel McGuire"
   {:abilities [{:label "Add an installed card to HQ"
                 :cost [:click 1]
                 :choices {:req installed?}
                 :msg (msg "move " (card-str state target) " to HQ")
                 :effect (effect (move target :hand))}]}})
