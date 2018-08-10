(in-ns 'game.cards.resources)

(def card-definition-personal-workshop
  {"Personal Workshop"
   (let [remove-counter
         {:req (req (not (empty? (:hosted card))))
          :once :per-turn
          :msg (msg "remove 1 counter from " (:title target))
          :choices {:req #(:host %)}
          :effect (req (if (zero? (get-counters (get-card state target) :power))
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
                                      num-counters (get-counters (get-card state paydowntarget) :power)]
                                  (resolve-ability
                                    state side
                                    {:prompt "How many counters to remove?"
                                     :choices {:number (req (min (:credit runner)
                                                                 num-counters))}
                                     :msg (msg "remove " target " counters from " (:title paydowntarget))
                                     :effect (req (do
                                                    (lose-credits state side target)
                                                    (if (= num-counters target)
                                                      (runner-install state side (dissoc paydowntarget :counter) {:no-cost true})
                                                      (add-counter state side paydowntarget :power (- target)))))}
                                    card nil)))}]
      :events {:runner-turn-begins remove-counter}})})
