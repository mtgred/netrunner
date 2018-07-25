(in-ns 'game.cards.operations)

(def card-definition-housekeeping
  {"Housekeeping"
   {:events {:runner-install {:player :runner
                              :prompt "Select a card from your Grip to trash for Housekeeping" :once :per-turn
                              :choices {:req #(and (= (:side %) "Runner")
                                                   (in-hand? %))}
                              :msg (msg "force the Runner to trash " (:title target) " from their Grip")
                              :effect (effect (trash target {:unpreventable true}))}}}})
