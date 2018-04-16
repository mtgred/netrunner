(in-ns 'game.core)

(declare run-event)

(def card-events-lucky-find
  {"Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain :credit 9))}})