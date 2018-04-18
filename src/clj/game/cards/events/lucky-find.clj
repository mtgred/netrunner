(in-ns 'game.core)

(def card-definitions-events-lucky-find
  {"Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain :credit 9))}})
