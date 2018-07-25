(in-ns 'game.cards.resources)

(def card-definition-power-tap
  {"Power Tap"
   {:events {:pre-init-trace {:msg "gain 1 [Credits]"
                              :effect (effect (gain-credits :runner 1))}}}})
