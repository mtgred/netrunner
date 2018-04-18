(in-ns 'game.core)

(def card-definitions-assets-pad-factory
  {"PAD Factory"
   {:abilities [{:cost [:click 1]
                 :label "Place 1 advancement token on a card"
                 :choices {:req installed?}
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                              (let [tgtcid (:cid target)]
                                (register-turn-flag! state side
                                  target :can-score
                                  (fn [state side card]
                                    (if (and (= (:cid card) tgtcid)
                                             (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
                                      ((constantly false) (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                      true)))))}]}})
