(in-ns 'game.core)

(def card-definitions-upgrades-signal-jamming
  {"Signal Jamming"
   {:abilities [{:label "[Trash]: Cards cannot be installed until the end of the run"
                 :msg (msg "prevent cards being installed until the end of the run")
                 :req (req this-server)
                 :effect (effect (trash (get-card state card) {:cause :ability-cost}))}]
    :trash-effect {:effect (effect (lock-install (:cid card) :runner)
                                   (lock-install (:cid card) :corp)
                                   (toast :runner "Cannot install until the end of the run")
                                   (toast :corp "Cannot install until the end of the run")
                                   (register-events {:run-ends {:effect (effect (unlock-install (:cid card) :runner)
                                                                                (unlock-install (:cid card) :corp))}}
                                                    (assoc card :zone '(:discard))))}
    :events {:run-ends nil
             :turn-ends {:effect (effect (unregister-events card))}}}})
