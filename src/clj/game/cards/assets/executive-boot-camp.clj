(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-executive-boot-camp
  {"Executive Boot Camp"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(not (rezzed? %)) (all-installed state :corp)))}
    :abilities [{:choices {:req (complement rezzed?)}
                 :label "Rez a card, lowering the cost by 1 [Credits]"
                 :msg (msg "rez " (:title target))
                 :effect (effect (rez-cost-bonus -1)
                                 (rez target {:no-warning true})
                                 (update! (assoc card :ebc-rezzed (:cid target))))}
                {:prompt "Choose an asset to add to HQ"
                 :msg (msg "add " (:title target) " to HQ")
                 :activatemsg "searches R&D for an asset"
                 :choices (req (cancellable (filter #(is-type? % "Asset")
                                                    (:deck corp))
                                            :sorted))
                 :cost [:credit 1]
                 :label "Search R&D for an asset"
                 :effect (effect (trash card)
                                 (shuffle! :deck)
                                 (move target :hand))}]

    ; A card rezzed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
    :suppress {:corp-turn-begins {:req (req (= (:cid target) (:ebc-rezzed (get-card state card))))}}
    :events {:corp-turn-ends {:req (req (:ebc-rezzed card))
                              :effect (effect (update! (dissoc card :ebc-rezzed)))}}}})