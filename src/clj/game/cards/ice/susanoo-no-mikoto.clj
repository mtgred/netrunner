(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-susanoo-no-mikoto
  {"Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:server run) [:discard]))
                   :msg "make the Runner continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                                 :server [:archives])))}]}})