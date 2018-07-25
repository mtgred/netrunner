(in-ns 'game.cards.programs)

(def card-definition-snitch
  {"Snitch"
   {:abilities [{:once :per-run :req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :async true
                 :effect (req (wait-for (expose state side current-ice)
                                        (continue-ability
                                          state side
                                          {:optional {:prompt "Jack out?"
                                                      :yes-ability {:msg "jack out"
                                                                    :effect (effect (jack-out nil))}
                                                      :no-ability {:msg "continue the run"}}}
                                          card nil)))}]}})
