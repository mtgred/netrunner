(in-ns 'game.core)

(def card-definitions-identities-the-shadow-pulling-the-strings
  {"The Shadow: Pulling the Strings"
   {:events {:pre-start-game {:effect draft-points-target}}}})
