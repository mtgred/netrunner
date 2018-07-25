(in-ns 'game.cards.programs)

(def card-definition-paintbrush
  {"Paintbrush"
   {:abilities [{:cost [:click 1]
                 :choices {:req #(and (installed? %) (ice? %) (rezzed? %))}
                 :effect (req (let [ice target
                                    stypes (:subtype ice)]
                           (resolve-ability
                              state :runner
                              {:prompt (msg "Choose a subtype")
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "spend [Click] and make " (card-str state ice) " gain " (.toLowerCase target)
                                         " until the end of the next run this turn")
                               :effect (effect (update! (assoc ice :subtype (combine-subtypes true stypes target)))
                                               (update-ice-strength (get-card state ice))
                                               (register-events {:run-ends
                                                                 {:effect (effect (update! (assoc ice :subtype stypes))
                                                                                  (unregister-events card)
                                                                                  (update-ice-strength (get-card state ice)))}} card))}
                            card nil)))}]
    :events {:run-ends nil}}})
