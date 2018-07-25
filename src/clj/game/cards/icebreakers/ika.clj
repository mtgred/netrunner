(in-ns 'game.cards.icebreakers)

(def card-definition-ika
  {"Ika"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 3)
                                 {:label "Host Ika on a piece of ICE"
                                  :prompt (msg "Host Ika on a piece of ICE")
                                  :cost [:credit 2]
                                  :choices {:req #(and (ice? %)
                                                       (installed? %)
                                                       (can-host? %))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))}]})})
