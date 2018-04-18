(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-authenticator
  {"Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag]
    :runner-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to Bypass it")
                                     (tag-runner state :runner eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits 2)
                  end-the-run]}})
