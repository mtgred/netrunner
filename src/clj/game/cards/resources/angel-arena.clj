(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-angel-arena
  {"Angel Arena"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [{:counter-cost [:power 1]
                 :msg "look at the top card of Stack"
                 :effect (req (when (zero? (get-in card [:counter :power] 0))
                                (trash state :runner card {:unpreventable true})))
                 :optional {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                            :yes-ability {:msg "add the top card of Stack to the bottom"
                                          :effect (req (move state side (first (:deck runner)) :deck))}}}]}})