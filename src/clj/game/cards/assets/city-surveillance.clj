(in-ns 'game.core)

(def card-definitions-assets-city-surveillance
  {"City Surveillance"
   {:events {:runner-turn-begins
             {:prompt "Pay 1 [Credits] or take 1 tag" :choices ["Pay 1 [Credits]" "Take 1 tag"]
              :player :runner :msg "make the Runner pay 1 [Credits] or take 1 tag"
              :delayed-completion true
              :effect (req (if-not (and (= target "Pay 1 [Credits]")
                                        (pay state side card :credit 1)
                                        (effect-completed state side eid))
                             (do (tag-runner state side eid 1)
                                 (system-msg state side "takes 1 tag"))
                             (system-msg state side "pays 1 [Credits]")))}}}})
