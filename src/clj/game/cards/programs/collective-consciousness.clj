(in-ns 'game.core)

(declare can-host?)

(def card-programs-collective-consciousness
  {"Collective Consciousness"
   {:events {:rez {:req (req (ice? target)) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}})
