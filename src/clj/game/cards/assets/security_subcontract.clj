(in-ns 'game.cards.assets)

(def card-definition-security-subcontract
  {"Security Subcontract"
   {:abilities [{:choices {:req #(and (ice? %)
                                      (rezzed? %))}
                 :cost [:click 1]
                 :msg (msg "trash " (:title target) " to gain 4 [Credits]")
                 :label "Trash a rezzed ICE to gain 4 [Credits]"
                 :effect (effect (trash target {:cause :ability-cost})
                                 (gain-credits 4))}]}})
