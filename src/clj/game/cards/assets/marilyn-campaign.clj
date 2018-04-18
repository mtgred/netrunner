(in-ns 'game.core)

(def card-definitions-assets-marilyn-campaign
  {"Marilyn Campaign"
   (let [ability {:msg "gain 2 [Credits]"
                  :counter-cost [:credit 2]
                  :once :per-turn
                  :req (req (:corp-phase-12 @state))
                  :label (str "Gain 2 [Credits] (start of turn)")
                  :effect (req (gain state :corp :credit 2)
                               (when (zero? (get-in card [:counter :credit]))
                                 (trash state :corp card)))}]
     {:effect (effect (add-counter card :credit 8))
      :flags {:corp-phase-12 (req (= 2 (get-in card [:counter :credit])))}
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]
      :trash-effect {:req (req (= :servers (first (:previous-zone card))))
                     :delayed-completion true
                     :effect (effect (show-wait-prompt :runner "Corp to use Marilyn Campaign")
                                     (continue-ability :corp
                                       {:optional
                                        {:prompt "Shuffle Marilyn Campaign into R&D?"
                                         :player :corp
                                         :yes-ability {:msg "shuffle it back into R&D"
                                                       :effect (req (move state :corp card :deck)
                                                                    (shuffle! state :corp :deck)
                                                                    (clear-wait-prompt state :runner)
                                                                    (effect-completed state side eid))}
                                         :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                                   (effect-completed state side eid))}}}
                                      card nil))}})})
