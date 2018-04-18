(in-ns 'game.core)

(def card-definitions-events-peace-in-our-time
  {"Peace in Our Time"
   {:req (req (not (:scored-agenda corp-reg)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :effect (req (gain state :runner :credit 10)
                 (gain state :corp :credit 5)
                 (apply prevent-run-on-server
                        state card (get-zones @state))
                 (register-events state side
                   {:runner-turn-ends {:effect (req (apply enable-run-on-server state card (get-zones @state)))}}
                  (assoc card :zone '(:discard))))
    :events {:runner-turn-ends nil}}})
