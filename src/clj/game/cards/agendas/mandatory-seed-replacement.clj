(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-mandatory-seed-replacement
  {"Mandatory Seed Replacement"
   (letfn [(msr [] {:prompt "Select two pieces of ICE to swap positions"
                    :choices {:req #(and (installed? %)
                                         (ice? %))
                              :max 2}
                    :delayed-completion true
                    :effect (req (if (= (count targets) 2)
                                   (do (swap-ice state side (first targets) (second targets))
                                       (system-msg state side
                                                   (str "swaps the position of "
                                                        (card-str state (first targets))
                                                        " and "
                                                        (card-str state (second targets))))
                                       (continue-ability state side (msr) card nil))
                                   (do (system-msg state :corp (str "has finished rearranging ICE"))
                                       (effect-completed state side eid))))})]
     {:delayed-completion true
      :msg "rearrange any number of ICE"
      :effect (effect (continue-ability (msr) card nil))})})