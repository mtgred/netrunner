(in-ns 'game.core)

(def card-operations-shipment-from-kaguya
  {"Shipment from Kaguya"
   {:choices {:max 2 :req can-be-advanced?}
    :msg (msg "place 1 advancement token on " (count targets) " cards")
    :effect (req (doseq [t targets] (add-prop state :corp t :advance-counter 1 {:placed true}))
                 (effect-completed state side eid card))}})
