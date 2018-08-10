(in-ns 'game.cards.resources)

(def card-definition-adjusted-chronotype
  {"Adjusted Chronotype"
   {:events {:runner-loss {:req (req (and (some #{:click} target)
                                          (let [click-losses (count (filter #(= :click %) (mapcat first (turn-events state side :runner-loss))))]
                                            (or (= 1 click-losses)
                                                (and (= 2 click-losses)
                                                     (has-flag? state side :persistent :genetics-trigger-twice))))))
                           :msg "gain [Click]"
                           :effect (effect (gain :runner :click 1))}}}})
