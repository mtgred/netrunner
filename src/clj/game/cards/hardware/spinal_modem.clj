(in-ns 'game.cards.hardware)

(def card-definition-spinal-modem
  {"Spinal Modem"
   {:in-play [:memory 1]
    :recurring 2
    :events {:successful-trace {:req (req run)
                                :effect (effect (system-msg (str "suffers 1 brain damage from Spinal Modem"))
                                                (damage eid :brain 1 {:card card}))}}}})
