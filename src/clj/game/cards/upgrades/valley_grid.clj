(in-ns 'game.cards.upgrades)

(def card-definition-valley-grid
  {"Valley Grid"
   {:implementation "Activation is manual"
    :abilities [{:req (req this-server)
                 :label "Reduce Runner's maximum hand size by 1 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (lose state :runner :hand-size 1))}]
    :trash-effect {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
                   :effect (req (when-let [n (:times-used card)]
                                  (register-events state side
                                                   {:corp-turn-begins
                                                    {:msg (msg "increase the Runner's maximum hand size by " n)
                                                     :effect (effect (gain :runner :hand-size {:mod n})
                                                                     (unregister-events card)
                                                                     (update! (dissoc card :times-used)))}}
                                                   (assoc card :zone '(:discard)))))}
    :events {:corp-turn-begins {:req (req (:times-used card))
                                :msg (msg "increase the Runner's maximum hand size by "
                                          (:times-used card))
                                :effect (effect (gain :runner :hand-size {:mod (:times-used card)})
                                                (update! (dissoc card :times-used)))}}}})
