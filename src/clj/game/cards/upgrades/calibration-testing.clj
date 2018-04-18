(in-ns 'game.core)

(def card-definitions-upgrades-calibration-testing
  {"Calibration Testing"
   {:abilities [{:label "[Trash]: Place 1 advancement token on a card in this server"
                 :delayed-completion true
                 :effect (effect (continue-ability
                                   {:prompt "Select a card in this server"
                                    :choices {:req #(in-same-server? % card)}
                                    :delayed-completion true
                                    :msg (msg "place an advancement token on " (card-str state target))
                                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                    (trash eid card {:cause :ability-cost}))}
                                   card nil))}]}})
