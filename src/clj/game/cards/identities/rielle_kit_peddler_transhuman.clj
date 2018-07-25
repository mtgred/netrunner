(in-ns 'game.cards.identities)

(def card-definition-rielle-kit-peddler-transhuman
  {"Rielle \"Kit\" Peddler: Transhuman"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed (get-card state current-ice))))
                 :once :per-turn
                 :msg (msg "make " (:title current-ice) " gain Code Gate until the end of the run")
                 :effect (req (let [ice current-ice
                                    stypes (:subtype ice)]
                                (update! state side (assoc ice :subtype (combine-subtypes true stypes "Code Gate")))
                                (register-events state side
                                                 {:run-ends {:effect (effect (update! (assoc ice :subtype stypes))
                                                                             (trigger-event :ice-subtype-changed ice)
                                                                             (unregister-events card))}} card)
                                (update-ice-strength state side ice)
                                (trigger-event state side :ice-subtype-changed ice)))}]
    :events {:run-ends nil}}})
