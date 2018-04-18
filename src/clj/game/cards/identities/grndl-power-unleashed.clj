(in-ns 'game.core)

(def card-definitions-identities-grndl-power-unleashed
  {"GRNDL: Power Unleashed"
   {:events {:pre-start-game {:req (req (= :corp side))
                              :effect (req (gain state :corp :credit 5)
                                           (when (= 0 (:bad-publicity corp))
                                             (gain-bad-publicity state :corp 1)))}}}})
