(in-ns 'game.cards.icebreakers)

(def card-definition-crypsis
  {"Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 1 "ICE" (effect (update! (assoc card :crypsis-broke true))))
                                 (strength-pump 1 1)
                                 {:cost [:click 1]
                                  :msg "place 1 virus counter"
                                  :effect (effect (add-counter card :virus 1))}]
                     :events (let [encounter-ends-effect
                                   {:req (req (:crypsis-broke card))
                                    :effect (req ((:effect breaker-auto-pump) state side eid card targets)
                                                 (if (pos? (get-counters card :virus))
                                                   (add-counter state side card :virus -1)
                                                   (trash state side card {:cause :self-trash}))
                                                 (update! state side (dissoc (get-card state card) :crypsis-broke)))}]
                               {:pass-ice encounter-ends-effect
                                :run-ends encounter-ends-effect})
                     :move-zone (req (when (= [:discard] (:zone card))
                                       (update! state side (dissoc card :crypsis-broke))))})})
