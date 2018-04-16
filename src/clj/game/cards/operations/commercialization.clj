(in-ns 'game.core)

(def card-operations-commercialization
  {"Commercialization"
   {:msg (msg "gain " (:advance-counter target 0) " [Credits]")
    :choices {:req ice?}
    :effect (final-effect (gain :credit (:advance-counter target 0)))}})