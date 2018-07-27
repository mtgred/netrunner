(in-ns 'game.cards.identities)

(def card-definition-tennin-institute-the-secrets-within
  {"Tennin Institute: The Secrets Within"
   {:flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                     (not-last-turn? state :runner :successful-run)))}
    :abilities [{:req (req (and (:corp-phase-12 @state)
                                (not-last-turn? state :runner :successful-run)))
                 :label "Place 1 advancement token"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req installed?}
                 :once :per-turn
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}})
