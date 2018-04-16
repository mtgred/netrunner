(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-bulwark
  {"Bulwark"
   {:effect take-bad-pub
    :abilities [{:msg "gain 2 [Credits] if there is an installed AI"
                 :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                 :effect (effect (gain :credit 2))}]
    :subroutines [(assoc trash-program :player :runner
                                       :msg "force the Runner to trash 1 program"
                                       :label "The Runner trashes 1 program")
                  {:msg "gain 2 [Credits] and end the run"
                   :effect (effect (gain :credit 2)
                                   (end-run))}]}})