(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-utopia-fragment
  {"Utopia Fragment"
   {:events {:pre-steal-cost {:req (req (pos? (:advance-counter target 0)))
                              :effect (req (let [counter (:advance-counter target)]
                                             (steal-cost-bonus state side [:credit (* 2 counter)])))}}}})