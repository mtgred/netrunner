(in-ns 'game.core)

(def card-definitions-identities-information-dynamics-all-you-need-to-know
  {"Information Dynamics: All You Need To Know"
   {:events (let [inf {:req (req (and (not (:disabled card))
                                      (has-most-faction? state :corp "NBN")))
                       :msg "give the Runner 1 tag"
                       :delayed-completion true
                       :effect (effect (tag-runner :runner eid 1))}]
              {:pre-start-game {:effect draft-points-target}
               :agenda-scored inf :agenda-stolen inf})}})
