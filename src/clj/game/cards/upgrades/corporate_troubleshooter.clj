(in-ns 'game.cards.upgrades)

(def card-definition-corporate-troubleshooter
  {"Corporate Troubleshooter"
   {:abilities [{:label "[Trash]: Add strength to a rezzed ICE protecting this server" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card :troubleshooter-target target
                                                                                :troubleshooter-amount boost))
                                                (trash state side (get-card state card))
                                                (update-ice-strength state side target))} card nil)))}]
    :events {:pre-ice-strength nil :runner-turn-ends nil :corp-turn-ends nil}
    :trash-effect
               {:effect (req (register-events
                               state side
                               (let [ct {:effect (req (unregister-events state side card)
                                                      (update! state side (dissoc card :troubleshooter-target))
                                                      (update-ice-strength state side (:troubleshooter-target card)))}]
                                 {:pre-ice-strength
                                                    {:req (req (= (:cid target) (:cid (:troubleshooter-target card))))
                                                     :effect (effect (ice-strength-bonus (:troubleshooter-amount card) target))}
                                  :runner-turn-ends ct :corp-turn-ends ct}) card))}}})
