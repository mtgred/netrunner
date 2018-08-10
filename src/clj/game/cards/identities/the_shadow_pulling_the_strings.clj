(in-ns 'game.cards.identities)

(def card-definition-the-shadow-pulling-the-strings
  {"The Shadow: Pulling the Strings"
   {:events {:pre-start-game {:effect draft-points-target}}}})
