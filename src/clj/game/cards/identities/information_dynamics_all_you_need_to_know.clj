(in-ns 'game.cards.identities)

(def card-definition-information-dynamics-all-you-need-to-know
  {"Information Dynamics: All You Need To Know"
   {:events (let [inf {:req (req (and (not (:disabled card))
                                      (has-most-faction? state :corp "NBN")))
                       :msg "give the Runner 1 tag"
                       :async true
                       :effect (effect (gain-tags :corp eid 1))}]
              {:pre-start-game {:effect draft-points-target}
               :agenda-scored inf :agenda-stolen inf})}})
