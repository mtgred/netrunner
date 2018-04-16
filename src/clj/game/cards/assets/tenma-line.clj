(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-tenma-line
  {"Tenma Line"
   {:abilities [{:label "Swap 2 pieces of installed ICE"
                 :cost [:click 1]
                 :prompt "Select two pieces of ICE to swap positions"
                 :choices {:req #(and (installed? %) (ice? %)) :max 2}
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg (msg "swap the positions of "
                           (card-str state (first targets))
                           " and "
                           (card-str state (second targets)))}]}})