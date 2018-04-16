(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-tapestry
  {"Tapestry"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:msg "draw 1 card"
                   :effect (effect (draw))}
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Choose a card in HQ to move to the top of R&D"
                   :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                   :msg "add 1 card in HQ to the top of R&D"
                   :effect (effect (move target :deck {:front true}))}]}})