(in-ns 'game.core)

(def card-definitions-assets-mr-stone
  {"Mr. Stone"
   {:events {:runner-gain-tag {:delayed-completion true
                               :msg "do 1 meat damage"
                               :effect (effect (damage :corp eid :meat 1 {:card card}))}}}})
