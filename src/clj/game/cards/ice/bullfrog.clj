(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-bullfrog
  {"Bullfrog"
   {:subroutines [(do-psi {:label "Move Bullfrog to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req servers)
                           :msg (msg "move it to the outermost position of " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                           :server (rest dest))))
                                        (move state side card
                                              (conj (server->zone state target) :ices)))})]}})
