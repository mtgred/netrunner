(in-ns 'game.cards.hardware)

(def card-definition-security-nexus
  {"Security Nexus"
   {:implementation "Bypass is manual"
    :in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :async true
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:base 5
                         :successful {:msg "give the Runner 1 tag and end the run"
                                      :effect (effect (gain-tags :runner eid 1)
                                                      (end-run))}
                         :unsuccessful {:msg "bypass the current ICE"}}}]}})
