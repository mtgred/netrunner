(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-titan-transnational-investing-in-your-future
  {"Titan Transnational: Investing In Your Future"
   {:events {:agenda-scored {:msg (msg "add 1 agenda counter to " (:title target))
                             :effect (effect (add-counter (get-card state target) :agenda 1))}}}})
