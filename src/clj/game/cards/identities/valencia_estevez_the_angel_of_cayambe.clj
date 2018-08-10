(in-ns 'game.cards.identities)

(def card-definition-valencia-estevez-the-angel-of-cayambe
  {"Valencia Estevez: The Angel of Cayambe"
   {:events {:pre-start-game
             {:req (req (and (= side :runner)
                             (zero? (get-in @state [:corp :bad-publicity]))))
              :effect (effect (gain :corp :bad-publicity 1))}}}})
