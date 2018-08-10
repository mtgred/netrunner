(in-ns 'game.cards.resources)

(def card-definition-bug-out-bag
  {"Bug Out Bag"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :events {:runner-turn-ends {:req (req (zero? (count (:hand runner))))
                                :msg (msg "draw " (get-counters card :power) " cards. Bug Out Bag is trashed")
                                :effect (effect (draw (get-counters card :power))
                                                (trash card))}}}})
