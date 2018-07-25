(in-ns 'game.cards.hardware)

(def card-definition-qianju-pt
  {"Qianju PT"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (update! (assoc card :qianju-active true)))
                 :msg "lose [Click] and avoid the first tag received until their next turn"}]
    :events {:corp-turn-ends {:effect (effect (update! (dissoc card :qianju-active)))}
             :runner-turn-begins {:req (req (:qianju-active card))
                                  :effect (effect (lose :click 1))}
             :pre-tag {:req (req (:qianju-active card))
                       :msg "avoid the first tag received"
                       :effect (effect (tag-prevent :runner 1)
                                       (update! (dissoc card :qianju-active)))}}}})
