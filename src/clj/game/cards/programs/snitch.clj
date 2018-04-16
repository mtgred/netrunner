(in-ns 'game.core)

(declare can-host?)

(def card-programs-snitch
  {"Snitch"
   {:abilities [{:once :per-run :req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :delayed-completion true
                 :effect (req (when-completed (expose state side current-ice)
                                              (continue-ability
                                                state side
                                                {:optional {:prompt "Jack out?"
                                                            :yes-ability {:msg "jack out"
                                                                          :effect (effect (jack-out nil))}
                                                            :no-ability {:msg "continue the run"}}}
                                                card nil)))}]}})