(in-ns 'game.core)

(def card-definitions-events-reshape
  {"Reshape"
   {:prompt "Select two non-rezzed ICE to swap positions"
    :choices {:req #(and (installed? %) (not (rezzed? %)) (ice? %)) :max 2}
    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
    :effect (req (when (= (count targets) 2)
                   (swap-ice state side (first targets) (second targets))))}})
