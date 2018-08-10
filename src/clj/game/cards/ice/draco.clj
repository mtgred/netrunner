(in-ns 'game.cards.ice)

(def card-definition-draco
  {"Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-ice-strength card))
    :strength-bonus (req (get-counters card :power))
    :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                    :msg "give the Runner 1 tag and end the run"
                                    :async true
                                    :effect (effect (gain-tags :corp eid 1)
                                                    (end-run))})]}})
