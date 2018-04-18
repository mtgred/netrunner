(in-ns 'game.core)

(def card-definitions-operations-defective-brainchips
  {"Defective Brainchips"
   {:events {:pre-damage {:req (req (= target :brain)) :msg "do 1 additional brain damage"
                          :once :per-turn :effect (effect (damage-bonus :brain 1))}}}})
