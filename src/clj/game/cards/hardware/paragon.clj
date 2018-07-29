(in-ns 'game.cards.hardware)

(def card-definition-paragon
  {"Paragon"
   {:in-play [:memory 1]
    :events {:successful-run
             {:req (req (first-event? state side :successful-run))
              :async true
              :effect (effect
                        (show-wait-prompt :corp "Runner to decide if they will use Paragon")
                        (continue-ability
                          {:optional
                           {:player :runner
                            :prompt "Use Paragon?"
                            :yes-ability
                            {:msg "gain 1 [Credit] and look at the top card of Stack"
                             :async true
                             :effect (effect
                                       (gain-credits :runner 1)
                                       (continue-ability
                                         {:player :runner
                                          :optional
                                          {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                                           :yes-ability
                                           {:msg "add the top card of Stack to the bottom"
                                            :effect (effect (move :runner (first (:deck runner)) :deck)
                                                            (clear-wait-prompt :corp))}
                                           :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                         card nil))}
                            :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                         (system-msg "does not add the top card of the Stack to the bottom"))}}}
                          card nil))}}}})
