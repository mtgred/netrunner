(in-ns 'game.core)

(def card-definitions-agendas-philotic-entanglement
  {"Philotic Entanglement"
   {:interactive (req true)
    :req (req (> (count (:scored runner)) 0))
    :msg (msg "do " (count (:scored runner)) " net damage")
    :effect (effect (damage eid :net (count (:scored runner)) {:card card}))}})
