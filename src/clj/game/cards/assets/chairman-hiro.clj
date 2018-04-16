(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-chairman-hiro
  {"Chairman Hiro"
   {:effect (effect (lose :runner :hand-size-modification 2))
    :leave-play (effect (gain :runner :hand-size-modification 2))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}}})