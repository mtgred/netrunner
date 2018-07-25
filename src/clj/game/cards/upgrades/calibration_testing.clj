(in-ns 'game.cards.upgrades)

(def card-definition-calibration-testing
  {"Calibration Testing"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:label "[Trash]: Place 1 advancement token on a card in this server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Select a card in this server"
                                    :choices {:req #(in-same-server? % card)}
                                    :async true
                                    :msg (msg "place an advancement token on " (card-str state target))
                                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                    (trash eid card {:cause :ability-cost}))}
                                   card nil))}]}})
