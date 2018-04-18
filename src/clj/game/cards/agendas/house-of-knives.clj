(in-ns 'game.core)

(def card-definitions-agendas-house-of-knives
  {"House of Knives"
   {:effect (effect (add-counter card :agenda 3))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :msg "do 1 net damage"
                 :req (req (:run @state))
                 :once :per-run
                 :effect (effect (damage eid :net 1 {:card card}))}]}})
