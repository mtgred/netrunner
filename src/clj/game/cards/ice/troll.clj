(in-ns 'game.core)

(def card-definitions-ice-troll
  {"Troll"
   {:implementation "Encounter effect is manual"
    :abilities [(trace-ability 2 {:label "Force the Runner to lose [Click] or end the run"
                                  :msg "force the Runner to lose [Click] or end the run"
                                  :player :runner
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (can-pay? state :runner nil [:click 1]))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (do (lose state side :click 1)
                                                     (system-msg state side "loses [Click]"))))})]}})
