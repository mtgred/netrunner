(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-adjusted-chronotype
  {"Adjusted Chronotype"
   {:events {:runner-loss {:req (req (and (some #{:click} target)
                                          (let [click-losses (filter #(= :click %) (mapcat first (turn-events state side :runner-loss)))]
                                            (or (empty? click-losses)
                                                (and (= (count click-losses) 1)
                                                     (has-flag? state side :persistent :genetics-trigger-twice))))))
                           :msg "gain [Click]" :effect (effect (gain :runner :click 1))}}}})
