(in-ns 'game.core)

(def card-operations-shipment-from-tennin
  {"Shipment from Tennin"
   {:delayed-completion true
    :req (req (not-last-turn? state :runner :successful-run))
    :choices {:req #(and (installed? %) (= (:side %) "Corp"))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}})
