(in-ns 'game.cards.identities)

(def card-definition-boris-syfr-kovac-crafty-veteran
  {"Boris \"Syfr\" Kovac: Crafty Veteran"
   {:events {:pre-start-game {:effect draft-points-target}
             :runner-turn-begins {:req (req (and (has-most-faction? state :runner "Criminal")
                                                 (pos? (:tag runner))))
                                  :msg "remove 1 tag"
                                  :effect (effect (lose-tags 1))}}}})
