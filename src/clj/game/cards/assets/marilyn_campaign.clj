(in-ns 'game.cards.assets)

(def card-definition-marilyn-campaign
  {"Marilyn Campaign"
   (let [ability {:msg "gain 2 [Credits]"
                  :counter-cost [:credit 2]
                  :once :per-turn
                  :req (req (:corp-phase-12 @state))
                  :label (str "Gain 2 [Credits] (start of turn)")
                  :async true
                  :effect (req (take-credits state :corp 2)
                               (if (zero? (get-counters (get-card state card) :credit))
                                 (trash state :corp eid card {:unpreventable true})
                                 (effect-completed state :corp eid)))}]
     {:effect (effect (add-counter card :credit 8))
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :trash-effect {:req (req (= :servers (first (:previous-zone card))))
                     :async true
                     :effect (effect (show-wait-prompt :runner "Corp to use Marilyn Campaign")
                                     (continue-ability :corp
                                       {:optional
                                        {:prompt "Shuffle Marilyn Campaign into R&D?"
                                         :priority 1
                                         :player :corp
                                         :yes-ability {:msg "shuffle it back into R&D"
                                                       :effect (req (move state :corp card :deck)
                                                                    (shuffle! state :corp :deck)
                                                                    (effect-completed state side eid))}
                                         :end-effect (effect (clear-wait-prompt :runner))}}
                                      card nil))}})})
