(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-inazuma
  {"Inazuma"
   {:abilities [{:msg "prevent the Runner from breaking subroutines on the next piece of ICE they encounter this run"}
                {:msg "prevent the Runner from jacking out until after the next piece of ICE"
                 :effect (effect (register-events
                                   {:pass-ice {:effect (req (swap! state update-in [:run] dissoc :prevent-jack-out)
                                                            (unregister-events state side card))}} card)
                                 (prevent-jack-out))}]}})