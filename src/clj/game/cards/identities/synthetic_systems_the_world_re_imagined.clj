(in-ns 'game.cards.identities)

(def card-definition-synthetic-systems-the-world-re-imagined
  {"Synthetic Systems: The World Re-imagined"
   {:events {:pre-start-game {:effect draft-points-target}}
    :flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                     (has-most-faction? state :corp "Jinteki")
                                     (> (count (filter ice? (all-installed state :corp))) 1)))}
    :abilities [{:prompt "Select two pieces of ICE to swap positions"
                 :label "Swap 2 pieces of ice"
                 :choices {:req #(and (installed? %)
                                      (ice? %))
                           :max 2}
                 :once :per-turn
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg (msg "swap the positions of " (card-str state (first targets))
                           " and " (card-str state (second targets)))}]}})
