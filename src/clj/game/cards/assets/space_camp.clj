(in-ns 'game.cards.assets)

(def card-definition-space-camp
  {"Space Camp"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :runner "Corp to use Space Camp")
                             (continue-ability
                               {:optional
                                {:prompt "Place 1 advancement token with Space Camp?"
                                 :cancel-effect (req (clear-wait-prompt state :runner)
                                                     (effect-completed state side eid))
                                 :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                                               :prompt "Select a card to place an advancement token on with Space Camp"
                                               :choices {:req can-be-advanced?}
                                               :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                               (clear-wait-prompt :runner))}
                                 :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid))}}}
                               card nil))}}})
