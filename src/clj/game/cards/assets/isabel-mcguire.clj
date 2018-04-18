(in-ns 'game.core)

(def card-definitions-assets-isabel-mcguire
  {"Isabel McGuire"
   {:abilities [{:cost [:click 1] :label "Add an installed card to HQ"
                 :choices {:req installed?}
                 :msg (msg "move " (card-str state target) " to HQ")
                 :effect (effect (move target :hand))}]}})
