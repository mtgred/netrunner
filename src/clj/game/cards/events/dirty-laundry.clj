(in-ns 'game.core)

(declare run-event)

(def card-events-dirty-laundry
  {"Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain :runner :credit 5))}})})
