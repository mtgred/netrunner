(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-loki
  {"Loki"
   {:implementation "Encounter effects not implemented"
    :subroutines [{:label "End the run unless the Runner shuffles their Grip into the Stack"
                   :effect (req (if (zero? (count (:hand runner)))
                                    (do (end-run state side)
                                        (system-msg state :corp (str "uses Loki to end the run")))
                                    (do (show-wait-prompt state :corp "Runner to decide to shuffle their Grip into the Stack")
                                        (resolve-ability state :runner
                                          {:optional
                                           {:prompt "Reshuffle your Grip into the Stack?"
                                            :player :runner
                                            :yes-ability {:effect (req (doseq [c (:hand runner)]
                                                                         (move state :runner c :deck))
                                                                       (shuffle! state :runner :deck)
                                                                       (system-msg state :runner (str "shuffles their Grip into their Stack"))
                                                                       (clear-wait-prompt state :corp))}
                                            :no-ability {:effect (effect (end-run)
                                                                         (system-msg :runner (str "doesn't shuffle their Grip into their Stack. Loki ends the run"))
                                                                         (clear-wait-prompt :corp))}}}
                                         card nil))))}]}})
