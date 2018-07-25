(in-ns 'game.cards.resources)

(def card-definition-lewi-guilherme
  {"Lewi Guilherme"
   (let [ability {:once :per-turn
                  :optional {:once :per-turn
                             :prompt "Pay 1 [Credits] to keep Lewi Guilherme?"
                             :yes-ability {:effect (req (if (pos? (:credit runner))
                                                          (do (lose-credits state side 1)
                                                              (system-msg state side "pays 1 [Credits] to keep Lewi Guilherme"))
                                                          (do (trash state side card)
                                                              (system-msg state side "must trash Lewi Guilherme"))))}
                             :no-ability {:effect (effect (trash card)
                                                          (system-msg "chooses to trash Lewi Guilherme"))}}}]
   {:flags {:drip-economy true ;; for Drug Dealer
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :drip-economy true)
                                                      (all-active-installed state :runner)))))}
    ;; KNOWN ISSUE: :effect is not fired when Assimilator turns cards over or Dr. Lovegood re-enables it.
    :effect (effect (lose :corp :hand-size 1))
    :leave-play (effect (gain :corp :hand-size 1))
    :abilities [(assoc-in ability [:req] (req (:runner-phase-12 @state)))]
    :events {:runner-turn-begins ability}})})
