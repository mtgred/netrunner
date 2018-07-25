(in-ns 'game.cards.identities)

(def card-definition-wyvern-chemically-enhanced
  {"Wyvern: Chemically Enhanced"
   {:events {:pre-start-game {:effect draft-points-target}
             :runner-trash {:req (req (and (has-most-faction? state :runner "Anarch")
                                           (card-is? target :side :corp)
                                           (pos? (count (:discard runner)))))
                            :msg (msg "shuffle " (:title (last (:discard runner))) " into their Stack")
                            :effect (effect (move :runner (last (:discard runner)) :deck)
                                            (shuffle! :runner :deck))}}}})
