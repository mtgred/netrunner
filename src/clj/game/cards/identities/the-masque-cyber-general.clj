(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-the-masque-cyber-general
  {"The Masque: Cyber General"
   {:events {:pre-start-game {:effect draft-points-target}}}})