(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-encryption-protocol
  {"Encryption Protocol"
   {:events {:pre-trash {:req (req (installed? target))
                         :effect (effect (trash-cost-bonus 1))}}}})