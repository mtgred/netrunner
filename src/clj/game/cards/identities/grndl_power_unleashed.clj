(in-ns 'game.cards.identities)

(def card-definition-grndl-power-unleashed
  {"GRNDL: Power Unleashed"
   {:events {:pre-start-game {:req (req (= :corp side))
                              :effect (req (gain-credits state :corp 5)
                                           (when (zero? (:bad-publicity corp))
                                             (gain-bad-publicity state :corp 1)))}}}})
