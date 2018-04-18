(in-ns 'game.core)

(def card-definitions-resources-dr-lovegood
  {"Dr. Lovegood"
   {:flags {:runner-phase-12 (req (> (count (all-installed state :runner)) 1))}
    :abilities [{:req (req (:runner-phase-12 @state))
                 :prompt "Select an installed card to make its text box blank for the remainder of the turn"
                 :once :per-turn
                 :choices {:req installed?}
                 :msg (msg "make the text box of " (:title target) " blank for the remainder of the turn")
                 :effect (req (let [c target]
                                (disable-card state side c)
                                (register-events state side
                                                 {:post-runner-turn-ends
                                                  {:effect (req (enable-card state side (get-card state c))
                                                                (when-let [reactivate-effect (:reactivate (card-def c))]
                                                                  (resolve-ability state :runner reactivate-effect
                                                                                   (get-card state c) nil))
                                                                (unregister-events state side card))}} card)))}]
    :events {:post-runner-turn-ends nil}}})
