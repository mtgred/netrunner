(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-waiver
  {"Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Runner's Grip and trash cards"
                                    :msg (msg "reveal all cards in the Runner's Grip: " (join ", " (map :title (:hand runner)))
                                              ". Cards with a play/install cost less than or equal to " (- target (second targets))
                                              " will be trashed")
                                    :effect (req (let [delta (- target (second targets))]
                                                   (doseq [c (:hand runner)]
                                                     (when (<= (:cost c) delta)
                                                       (resolve-ability
                                                         state side
                                                         {:msg (msg "trash " (:title c))
                                                          :effect (effect (trash c))}
                                                         card nil)))))})]}})
