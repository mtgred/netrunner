(in-ns 'game.cards.icebreakers)

(def card-definition-faerie
  {"Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 0 1 "Sentry" (effect (update! (assoc-in card [:special :faerie-used] true))))
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (get-in card [:special :faerie-used]))
                                         :effect (effect (trash card))}}})})
