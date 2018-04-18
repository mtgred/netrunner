(in-ns 'game.core)

(def card-definitions-icebreakers-puffer
  {"Puffer"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Memory use must be manually tracked by the Runner"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)
                                 {:cost [:click 1] :msg "place one power counter"
                                  :label "Place 1 power counter"
                                  :effect (effect (add-counter card :power 1)
                                                  (update-breaker-strength card))}
                                 {:cost [:click 1] :msg "remove one power counter"
                                  :label "Remove 1 power counter"
                                  :effect (effect (add-counter card :power -1)
                                                  (update-breaker-strength card))}]
                     :strength-bonus (req (get-in card [:counter :power] 0))})})
