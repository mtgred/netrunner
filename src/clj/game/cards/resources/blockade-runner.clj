(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-blockade-runner
  {"Blockade Runner"
   {:abilities [{:cost [:click 2]
                 :msg "draw 3 cards and shuffle 1 card from their Grip back into their Stack"
                 :effect (effect (draw 3)
                                 (resolve-ability
                                   {:prompt "Choose a card in your Grip to shuffle back into your Stack"
                                    :choices {:req #(and (in-hand? %)
                                                         (= (:side %) "Runner"))}
                                    :effect (effect (move target :deck)
                                                    (shuffle! :deck))}
                                  card nil))}]}})