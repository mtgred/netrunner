(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-watchtower
  {"Watchtower"
   {:subroutines [{:label "Search R&D and add 1 card to HQ"
                   :prompt "Choose a card to add to HQ"
                   :msg "add a card from R&D to HQ"
                   :choices (req (cancellable (:deck corp) :sorted))
                   :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                   :effect (effect (shuffle! :deck)
                                   (move target :hand))}]}})