(in-ns 'game.core)

(def card-definitions-programs-collective-consciousness
  {"Collective Consciousness"
   {:events {:rez {:req (req (ice? target)) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}})
