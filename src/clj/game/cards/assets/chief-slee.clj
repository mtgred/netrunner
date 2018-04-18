(in-ns 'game.core)

(def card-definitions-assets-chief-slee
  {"Chief Slee"
   {:abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg (str "adds 1 power counter to Chief Slee")))}
                {:counter-cost [:power 5] :cost [:click 1]
                 :delayed-completion true
                 :msg "do 5 meat damage"
                 :effect (effect (damage eid :meat 5 {:card card}))}]}})
