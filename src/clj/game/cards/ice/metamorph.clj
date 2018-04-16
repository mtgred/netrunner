(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-metamorph
  {"Metamorph"
   {:subroutines [{:label "Swap two ICE or swap two installed non-ICE"
                   :msg "swap two ICE or swap two installed non-ICE"
                   :delayed-completion true
                   :prompt "Choose one"
                   :choices ["Swap two ICE" "Swap two non-ICE"]
                   :effect (req (if (= target "Swap two ICE")
                                  (continue-ability state side {:prompt "Select the two ICE to swap"
                                                                :delayed-completion true
                                                                :choices {:req #(and (installed? %) (ice? %)) :max 2 :all true}
                                                                :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                                                :effect (req (when (= (count targets) 2)
                                                                               (swap-ice state side (first targets) (second targets))
                                                                               (effect-completed state side eid card)))} card nil)
                                  (continue-ability state side {:prompt "Select the two cards to swap"
                                                                :delayed-completion true
                                                                :choices {:req #(and (installed? %) (not (ice? %))) :max 2 :all true}
                                                                :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                                                :effect (req (when (= (count targets) 2)
                                                                               (swap-installed state side (first targets) (second targets))
                                                                               (effect-completed state side eid card)))} card nil)))}]}})