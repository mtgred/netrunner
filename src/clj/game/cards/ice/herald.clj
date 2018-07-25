(in-ns 'game.cards.ice)

(def card-definition-herald
  {"Herald"
   {:flags {:rd-reveal (req true)}
    :subroutines [(gain-credits-sub 2)
                  {:label "Pay 1 [Credits] to place 1 advancement token on a card that can be advanced"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req can-be-advanced?}
                   :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Herald subroutines")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Herald. Allow its subroutines to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (play-subroutine :corp eid {:card card :subroutine 0})
                                                                       (play-subroutine :corp eid {:card card :subroutine 1}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}})
