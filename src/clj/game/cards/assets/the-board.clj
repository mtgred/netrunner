(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-the-board
  {"The Board"
   {:effect (effect (lose :runner :agenda-point (count (:scored runner))))
    :leave-play (effect (gain :runner :agenda-point (count (:scored runner))))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}
    :events {:agenda-stolen {:effect (effect (lose :runner :agenda-point 1))}
             :card-moved {:req (req (or (some #{:scored} (:zone (first targets)))
                                        (some #{:scored} (:zone (second targets)))))
                          :effect (effect ((if (some #{:scored} (:zone (first targets))) gain lose) :runner :agenda-point 1))}}}})