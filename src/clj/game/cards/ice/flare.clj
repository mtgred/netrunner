(in-ns 'game.core)

(def card-definitions-ice-flare
  {"Flare"
   {:subroutines [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                    :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                    :delayed-completion true
                                    :effect (effect (continue-ability
                                                     {:prompt "Select a piece of hardware to trash"
                                                      :label "Trash a piece of hardware"
                                                      :choices {:req #(is-type? % "Hardware")}
                                                      :msg (msg "trash " (:title target))
                                                      :effect (req (when-completed
                                                                     (trash state side target {:cause :subroutine})
                                                                     (do (damage state side eid :meat 2 {:unpreventable true
                                                                                              :card card})
                                                                         (end-run state side))))
                                                      :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                             (end-run))}
                                                     card nil))})]}})
