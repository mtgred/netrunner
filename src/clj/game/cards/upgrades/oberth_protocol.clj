(in-ns 'game.cards.upgrades)

(def card-definition-oberth-protocol
  {"Oberth Protocol"
   {:additional-cost [:forfeit]
    :events {:advance {:req (req (and (same-server? card target)
                                      (= 1 (count (filter #(= (second (:zone %)) (second (:zone card)))
                                                          (map first (turn-events state side :advance)))))))
                       :msg (msg "place an additional advancement token on " (card-str state target))
                       :effect (effect (add-prop :corp target :advance-counter 1 {:placed true}))}}}})
