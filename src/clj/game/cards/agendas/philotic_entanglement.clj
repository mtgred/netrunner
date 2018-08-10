(in-ns 'game.cards.agendas)

(def card-definition-philotic-entanglement
  {"Philotic Entanglement"
   {:interactive (req true)
    :req (req (pos? (count (:scored runner))))
    :msg (msg "do " (count (:scored runner)) " net damage")
    :effect (effect (damage eid :net (count (:scored runner)) {:card card}))}})
