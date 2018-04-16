(in-ns 'game.core)

(declare run-event)

(def card-events-sure-gamble
  {"Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}})