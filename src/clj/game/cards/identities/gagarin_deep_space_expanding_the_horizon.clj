(in-ns 'game.cards.identities)

(def card-definition-gagarin-deep-space-expanding-the-horizon
  {"Gagarin Deep Space: Expanding the Horizon"
   {:flags {:slow-remote-access (req (not (:disabled card)))}
    :events {:pre-access-card {:req (req (is-remote? (second (:zone target))))
                               :effect (effect (access-cost-bonus [:credit 1]))
                               :msg "make the Runner spend 1 [Credits] to access"}}}})
