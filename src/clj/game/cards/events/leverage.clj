(in-ns 'game.core)

(declare run-event)

(def card-events-leverage
  {"Leverage"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :player :corp
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain-bad-publicity state :corp 2)
                       (system-msg state :corp "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :runner-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :runner "is immune to damage until the beginning of the Runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:runner-turn-begins nil :pre-damage nil}}})