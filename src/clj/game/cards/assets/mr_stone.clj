(in-ns 'game.cards.assets)

(def card-definition-mr-stone
  {"Mr. Stone"
   {:events {:runner-gain-tag {:async true
                               :msg "do 1 meat damage"
                               :effect (effect (damage :corp eid :meat 1 {:card card}))}}}})
