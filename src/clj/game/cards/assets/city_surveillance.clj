(in-ns 'game.cards.assets)

(def card-definition-city-surveillance
  {"City Surveillance"
   {:derezzed-events {:corp-turn-ends corp-rez-toast}
    :flags {:runner-phase-12 (req (pos? (:credit runner)))}
    :events {:runner-turn-begins
             {:player :runner
              :prompt "Pay 1 [Credits] or take 1 tag"
              :choices (req (concat (when (pos? (:credit runner))
                                      ["Pay 1 [Credits]"])
                                    ["Take 1 tag"]))
              :msg "make the Runner pay 1 [Credits] or take 1 tag"
              :async true
              :effect (req (case target
                             "Pay 1 [Credits]"
                             (do (system-msg state :runner "pays 1 [Credits]")
                                 (pay state :runner card :credit 1)
                                 (effect-completed state side eid))
                             (do (system-msg state :runner "takes 1 tag")
                                 (gain-tags state :corp eid 1))))}}}})
