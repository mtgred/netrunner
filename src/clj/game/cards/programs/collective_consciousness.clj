(in-ns 'game.cards.programs)

(def card-definition-collective-consciousness
  {"Collective Consciousness"
   {:events {:rez {:req (req (ice? target)) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}})
