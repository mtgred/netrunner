(in-ns 'game.cards.identities)

(def card-definition-the-masque-cyber-general
  {"The Masque: Cyber General"
   {:events {:pre-start-game {:effect draft-points-target}}}})
