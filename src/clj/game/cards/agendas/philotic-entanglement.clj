(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-philotic-entanglement
  {"Philotic Entanglement"
   {:interactive (req true)
    :req (req (> (count (:scored runner)) 0))
    :msg (msg "do " (count (:scored runner)) " net damage")
    :effect (effect (damage eid :net (count (:scored runner)) {:card card}))}})