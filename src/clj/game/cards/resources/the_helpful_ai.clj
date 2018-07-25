(in-ns 'game.cards.resources)

(def card-definition-the-helpful-ai
  {"The Helpful AI"
   {:in-play [:link 1]
    :abilities [{:msg (msg "give +2 strength to " (:title target))
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (installed? %))}
                 :effect (effect (update! (assoc card :hai-target target))
                                 (trash (get-card state card) {:cause :ability-cost})
                                 (update-breaker-strength target))}]
    :events {:runner-turn-ends nil :corp-turn-ends nil :pre-breaker-strength nil}
    :trash-effect {:effect
                   (effect (register-events
                             (let [hai {:effect (effect (unregister-events card)
                                                        (update! (dissoc card :hai-target))
                                                        (update-breaker-strength (:hai-target card)))}]
                               {:runner-turn-ends hai :corp-turn-ends hai
                                :pre-breaker-strength {:req (req (= (:cid target)(:cid (:hai-target card))))
                                                       :effect (effect (breaker-strength-bonus 2))}}) card))}}})
