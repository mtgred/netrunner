(in-ns 'game.cards.operations)

(def card-definition-trojan-horse
  {"Trojan Horse"
   {:req (req (:accessed-cards runner-reg))
    :trace {:base 4
            :label "Trace 4 - Trash a program"
            :successful {:async true
                         :effect (req (let [exceed (- target (second targets))]
                                        (continue-ability
                                          state side
                                          {:async true
                                           :prompt (str "Select a program with an install cost of no more than "
                                                        exceed "[Credits]")
                                           :choices {:req #(and (is-type? % "Program")
                                                                (installed? %)
                                                                (>= exceed (:cost %)))}
                                           :msg (msg "trash " (card-str state target))
                                           :effect (effect (trash eid target nil))}
                                          card nil)))}}}})
