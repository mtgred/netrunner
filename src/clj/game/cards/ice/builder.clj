(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-builder
  {"Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :ices)))}]
    :subroutines [{:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req #(and (ice? %)
                                        (can-be-advanced? %))}
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}})
