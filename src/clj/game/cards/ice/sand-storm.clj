(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-sand-storm
  {"Sand Storm"
   {:subroutines [{:req (req (:run @state))
                   :label "Move Sand Storm and the run to another server"
                   :prompt "Choose another server and redirect the run to its outermost position"
                   :choices (req (cancellable servers))
                   :msg (msg "move Sand Storm and the run.  The Runner is now running on " target ". Sand Storm is trashed")
                   :effect (req (let [dest (server->zone state target)]
                                  (swap! state update-in [:run]
                                         #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                 :server (rest dest)))
                                  (trash state side card {:unpreventable true})))}]}})
