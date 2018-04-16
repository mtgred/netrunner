(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-dean-lister
  {"Dean Lister"
   {:abilities [{:req (req (:run @state))
                 :msg (msg "add +1 strength for each card in their Grip to " (:title target) " until the end of the run")
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (installed? %))}
                 :effect (effect (update! (assoc card :dean-target target))
                                 (trash (get-card state card) {:cause :ability-cost})
                                 (update-breaker-strength target))}]
    :events {:run-ends nil :pre-breaker-strength nil}
    :trash-effect {:effect
                   (effect (register-events
                             (let [dean {:effect (effect (unregister-events card)
                                                         (update! (dissoc card :dean-target))
                                                         (update-breaker-strength (:dean-target card)))}]
                               {:run-ends dean
                                :pre-breaker-strength {:req (req (= (:cid target)(:cid (:dean-target card))))
                                                       :effect (effect (breaker-strength-bonus (count (:hand runner))))}}) card))}}})