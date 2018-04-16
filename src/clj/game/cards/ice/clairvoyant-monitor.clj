(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-clairvoyant-monitor
  {"Clairvoyant Monitor"
   {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                           :player :corp
                           :prompt "Select a target for Clairvoyant Monitor"
                           :msg (msg "place 1 advancement token on "
                                     (card-str state target) " and end the run")
                           :choices {:req installed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run))})]}})