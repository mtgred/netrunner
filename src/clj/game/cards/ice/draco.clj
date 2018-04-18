(in-ns 'game.core)

(def card-definitions-ice-draco
  {"Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-ice-strength card))
    :strength-bonus (req (get-in card [:counter :power] 0))
    :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                    :msg "give the Runner 1 tag and end the run"
                                    :delayed-completion true
                                    :effect (effect (tag-runner :runner eid 1)
                                                    (end-run))})]}})
