(in-ns 'game.cards.hardware)

(def card-definition-patchwork
  {"Patchwork"
   (letfn [(patchwork-discount [cost-type bonus-fn]
             {:async true
              :req (req (and (get-in card [:special :patchwork])
                             (= "Runner" (:side target))
                             ;; We need at least one card (that is not the card played) in hand
                             (not-empty (remove (partial same-card? target) (:hand runner)))))
              :effect (req (let [playing target]
                             (continue-ability
                               state side
                               {:prompt (str "Trash a card to lower the " cost-type " cost of " (:title playing) " by 2 [Credits].")
                                :priority 2
                                :choices {:req #(and (in-hand? %)
                                                     (= "Runner" (:side %))
                                                     (not (same-card? % playing)))}
                                :msg (msg "trash " (:title target) " to lower the " cost-type " cost of "
                                          (:title playing) " by 2 [Credits]")
                                :effect (effect (trash target {:unpreventable true})
                                             (bonus-fn [:credit -2])
                                             (update! (dissoc-in card [:special :patchwork])))
                                :cancel-effect (effect (effect-completed eid))}
                               card nil)))})]
     {:in-play [:memory 1]
      :implementation "Click Patchwork before playing/installing a card."
      :abilities [{:once :per-turn
                   :effect (effect (update! (assoc-in card [:special :patchwork] true))
                             (toast "Your next card played will trigger Patchwork." "info"))}]
      :events {:pre-play-instant (patchwork-discount "play" play-cost-bonus)
               :pre-install (patchwork-discount "install" install-cost-bonus)
               :runner-turn-ends {:effect (effect (update! (dissoc-in card [:special :patchwork])))}
               :corp-turn-ends {:effect (effect (update! (dissoc-in card [:special :patchwork])))}}})})
