(in-ns 'game.core)

(def card-definitions-icebreakers-faerie
  {"Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 0 1 "Sentry" (effect (update! (assoc card :faerie-used true))))
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (:faerie-used card))
                                         :effect (effect (trash (dissoc card :faerie-used)))}}})})
