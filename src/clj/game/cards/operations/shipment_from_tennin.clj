(in-ns 'game.cards.operations)

(def card-definition-shipment-from-tennin
  {"Shipment from Tennin"
   {:async true
    :req (req (not-last-turn? state :runner :successful-run))
    :choices {:req #(and (installed? %) (= (:side %) "Corp"))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}})
