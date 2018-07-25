(in-ns 'game.cards.assets)

(def card-definition-tenma-line
  {"Tenma Line"
   {:abilities [{:label "Swap 2 pieces of installed ICE"
                 :cost [:click 1]
                 :prompt "Select two pieces of ICE to swap positions"
                 :choices {:req #(and (installed? %)
                                      (ice? %))
                           :max 2}
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg (msg "swap the positions of "
                           (card-str state (first targets))
                           " and "
                           (card-str state (second targets)))}]}})
