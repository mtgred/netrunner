(in-ns 'game.cards.events)

(def card-definition-dirty-laundry
  {"Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain-credits :runner 5))}})})
