(in-ns 'game.core)

(def card-operations-shipment-from-sansan
  {"Shipment from SanSan"
   {:choices ["0", "1", "2"]
    :prompt "How many advancement tokens?"
    :delayed-completion true
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req can-be-advanced?}
                      :msg (msg "place " c " advancement tokens on " (card-str state target))
                      :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}})