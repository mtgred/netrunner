(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-crypsis
  {"Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 1 "ICE" (effect (update! (assoc card :crypsis-broke true))))
                                 (strength-pump 1 1)
                                 {:cost [:click 1]
                                  :msg "place 1 virus counter"
                                  :effect (effect (add-counter card :virus 1))}]
                     :events (let [encounter-ends-effect {:req (req (:crypsis-broke card))
                                                          :effect (req ((:effect breaker-auto-pump) state side eid card targets)
                                                                       (if (pos? (get-in card [:counter :virus] 0))
                                                                         (add-counter state side card :virus -1)
                                                                         (trash state side card {:cause :self-trash}))
                                                                       (update! state side (dissoc (get-card state card) :crypsis-broke)))}]
                               {:pass-ice encounter-ends-effect
                                :run-ends encounter-ends-effect})
                     :move-zone (req (when (= [:discard] (:zone card))
                                       (update! state side (dissoc card :crypsis-broke))))})})
