(in-ns 'game.cards.operations)

(def card-definition-defective-brainchips
  {"Defective Brainchips"
   {:events {:pre-damage {:req (req (= target :brain))
                          :msg "do 1 additional brain damage"
                          :once :per-turn
                          :effect (effect (damage-bonus :brain 1))}}}})
