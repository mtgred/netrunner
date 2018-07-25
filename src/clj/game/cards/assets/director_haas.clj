(in-ns 'game.cards.assets)

(def card-definition-director-haas
  {"Director Haas"
   {:in-play [:click 1 :click-per-turn 1]
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :runner eid card 2))}}})
