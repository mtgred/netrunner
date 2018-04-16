(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-director-haas
  {"Director Haas"
   {:in-play [:click 1 :click-per-turn 1]
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}}})
