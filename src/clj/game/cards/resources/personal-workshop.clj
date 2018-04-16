(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-personal-workshop
  {"Personal Workshop"
   (let [remove-counter
         {:req (req (not (empty? (:hosted card))))
          :once :per-turn
          :msg (msg "remove 1 counter from " (:title target)) :choices {:req #(:host %)}
          :effect (req (if (<= (get-in target [:counter :power]) 1)
                         (runner-install state side (dissoc target :counter) {:no-cost true})
                         (add-counter state side target :power -1)))}]
     {:flags {:drip-economy true}
      :abilities [{:label "Host a program or piece of hardware" :cost [:click 1]
                   :prompt "Select a card to host on Personal Workshop"
                   :choices {:req #(and (#{"Program" "Hardware"} (:type %))
                                        (in-hand? %)
                                        (= (:side %) "Runner"))}
                   :effect (req (if (zero? (:cost target))
                                  (runner-install state side target)
                                  (host state side card
                                        (assoc target :counter {:power (:cost target)}))))
                   :msg (msg "host " (:title target) "")}
                  (assoc remove-counter
                    :label "Remove 1 counter from a hosted card (start of turn)"
                    :cost [:credit 1])
                  {:label "X[Credit]: Remove counters from a hosted card"
                   :choices {:req #(:host %)}
                   :req (req (not (empty? (:hosted card))))
                   :effect (req (let [paydowntarget target
                                      num-counters (get-in paydowntarget [:counter :power] 0)]
                                  (resolve-ability
                                    state side
                                    {:prompt "How many counters to remove?"
                                     :choices {:number (req (min (:credit runner)
                                                                 num-counters))}
                                     :msg (msg "remove " target " counters from " (:title paydowntarget))
                                     :effect (req (do
                                                    (lose state side :credit target)
                                                    (if (= num-counters target)
                                                      (runner-install state side (dissoc paydowntarget :counter) {:no-cost true})
                                                      (add-counter state side paydowntarget :power (- target)))))}
                                    card nil)))}]
      :events {:runner-turn-begins remove-counter}})})