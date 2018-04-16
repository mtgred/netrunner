(in-ns 'game.core)

(def card-hardware-llds-processor
  {"LLDS Processor"
   {:events
     (let [llds {:effect (req (let [cards (:llds-target card)]
                                (update! state side (dissoc card :llds-target))
                                (doseq [c cards]
                                (update-breaker-strength state side
                                                         (find-cid (:cid c) (all-active-installed state :runner))))))}]
       {:runner-turn-ends llds :corp-turn-ends llds
        :runner-install {:silent (req true)
                         :req (req (has-subtype? target "Icebreaker"))
                         :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                         (update-breaker-strength target))}
        :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                               :effect (effect (breaker-strength-bonus 1))}})}})
