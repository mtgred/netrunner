(in-ns 'game.cards.operations)

(def card-definition-hatchet-job
  {"Hatchet Job"
   {:trace {:base 5
            :successful {:choices {:req #(and (installed? %)
                                              (card-is? % :side :runner)
                                              (not (has-subtype? % "Virtual")))}
                         :msg "add an installed non-virtual card to the Runner's grip"
                         :effect (effect (move :runner target :hand true))}}}})
