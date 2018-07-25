(in-ns 'game.cards.upgrades)

(def card-definition-signal-jamming
  {"Signal Jamming"
   {:abilities [{:label "[Trash]: Cards cannot be installed until the end of the run"
                 :msg (msg "prevent cards being installed until the end of the run")
                 :req (req this-server)
                 :effect (effect (trash (get-card state card) {:cause :ability-cost}))}]
    :trash-effect {:effect (effect (register-run-flag! card :corp-lock-install (constantly true))
                                   (register-run-flag! card :runner-lock-install (constantly true))
                                   (toast :runner "Cannot install until the end of the run")
                                   (toast :corp "Cannot install until the end of the run"))}
    :events {:run-ends nil}}})
