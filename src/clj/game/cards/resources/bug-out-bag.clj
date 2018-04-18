(in-ns 'game.core)

(def card-definitions-resources-bug-out-bag
  {"Bug Out Bag"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :events {:runner-turn-ends {:req (req (zero? (count (:hand runner))))
                                :msg (msg "draw " (get-in card [:counter :power] 0) " cards. Bug Out Bag is trashed")
                                :effect (effect (draw (get-in card [:counter :power] 0))
                                                (trash card))}}}})
