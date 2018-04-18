(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-ravana-10
  {"Ravana 1.0"
   {:subroutines [{:label "Resolve a subroutine on another piece of rezzed bioroid ICE"
                   :choices {:req #(and (rezzed? %) (ice? %) (has-subtype? % "Bioroid"))}
                   :msg (msg "resolve a subroutine on " (:title target))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
