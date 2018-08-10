(in-ns 'game.cards.ice)

(def card-definition-hailstorm
  {"Hailstorm"
   {:subroutines [{:label "Remove a card in the Heap from the game"
                   :prompt "Choose a card in the Runner's Heap"
                   :choices (req (:discard runner))
                   :msg (msg "remove " (:title target) " from the game")
                   :effect (effect (move :runner target :rfg))}
                  end-the-run]}})
