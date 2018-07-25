(in-ns 'game.cards.agendas)

(def card-definition-mandatory-seed-replacement
  {"Mandatory Seed Replacement"
   (letfn [(msr [] {:prompt "Select two pieces of ICE to swap positions"
                    :choices {:req #(and (installed? %)
                                         (ice? %))
                              :max 2}
                    :async true
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
     {:async true
      :msg "rearrange any number of ICE"
      :effect (effect (continue-ability (msr) card nil))})})
