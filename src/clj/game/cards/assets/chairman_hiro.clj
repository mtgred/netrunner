(in-ns 'game.cards.assets)

(def card-definition-chairman-hiro
  {"Chairman Hiro"
   {:effect (effect (lose :runner :hand-size 2))
    :leave-play (effect (gain :runner :hand-size 2))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :runner eid card 2))}}})
