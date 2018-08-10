(in-ns 'game.cards.icebreakers)

(def card-definition-golden
  {"Golden"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 2 2 "Sentry")
                                 (strength-pump 2 4)
                                 {:label "Derez a Sentry and return Golden to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Sentry")))
                                  :msg (msg "derez " (:title current-ice) " and return Golden to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})})
