(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-the-shadow-pulling-the-strings
  {"The Shadow: Pulling the Strings"
   {:events {:pre-start-game {:effect draft-points-target}}}})