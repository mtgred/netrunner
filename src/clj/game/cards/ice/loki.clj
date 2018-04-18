(in-ns 'game.core)

(def card-definitions-ice-loki
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
