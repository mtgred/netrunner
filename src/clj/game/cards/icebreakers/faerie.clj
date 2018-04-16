(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-faerie
  {"Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 0 1 "Sentry" (effect (update! (assoc card :faerie-used true))))
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (:faerie-used card))
                                         :effect (effect (trash (dissoc card :faerie-used)))}}})})