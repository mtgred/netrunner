(in-ns 'game.core)

(def card-definitions-assets-encryption-protocol
  {"Encryption Protocol"
   {:events {:pre-trash {:req (req (installed? target))
                         :effect (effect (trash-cost-bonus 1))}}}})
