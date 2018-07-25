(in-ns 'game.cards.ice)

(def card-definition-metamorph
  {"Metamorph"
   {:subroutines [{:label "Swap two ICE or swap two installed non-ICE"
                   :msg "swap two ICE or swap two installed non-ICE"
                   :async true
                   :prompt "Choose one"
                   :choices ["Swap two ICE" "Swap two non-ICE"]
                   :effect (req (if (= target "Swap two ICE")
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two ICE to swap"
                                     :async true
                                     :choices {:req #(and (installed? %) (ice? %)) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-ice state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two cards to swap"
                                     :async true
                                     :choices {:req #(and (installed? %) (not (ice? %))) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-installed state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)))}]}})
