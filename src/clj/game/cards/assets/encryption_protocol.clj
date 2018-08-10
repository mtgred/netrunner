(in-ns 'game.cards.assets)

(def card-definition-encryption-protocol
  {"Encryption Protocol"
   {:events {:pre-trash {:req (req (installed? target))
                         :effect (effect (trash-cost-bonus 1))}}}})
