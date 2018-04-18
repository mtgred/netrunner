(in-ns 'game.core)

(def card-definitions-hardware-security-nexus
  {"Security Nexus"
   {:in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :delayed-completion true
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:base 5 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (tag-runner :runner eid 1) (end-run))
                         :unsuccessful {:msg "bypass the current ICE"}}}]}})
