(in-ns 'game.core)

(declare run-event)

(def card-events-easy-mark
  {"Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}})
