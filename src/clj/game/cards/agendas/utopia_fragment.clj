(in-ns 'game.cards.agendas)

(def card-definition-utopia-fragment
  {"Utopia Fragment"
   {:events {:pre-steal-cost {:req (req (pos? (get-counters target :advancement)))
                              :effect (req (let [counter (get-counters target :advancement)]
                                             (steal-cost-bonus state side [:credit (* 2 counter)])))}}}})
