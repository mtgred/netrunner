(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-valencia-estevez-the-angel-of-cayambe
  {"Valencia Estevez: The Angel of Cayambe"
   {:events {:pre-start-game
             {:req (req (and (= side :runner)
                             (zero? (get-in @state [:corp :bad-publicity]))))
              :effect (effect (gain-bad-publicity :corp 1))}}}})