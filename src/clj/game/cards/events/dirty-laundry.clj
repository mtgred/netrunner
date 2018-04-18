(in-ns 'game.core)

(def card-definitions-events-dirty-laundry
  {"Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain :runner :credit 5))}})})
