(in-ns 'game.cards.assets)

(def card-definition-neurostasis
  {"Neurostasis"
   (advance-ambush 3 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :async true
                      :effect (req (let [cnt (get-counters (get-card state card) :advancement)]
                                     (continue-ability
                                       state side
                                       {:prompt (msg "Choose " (quantify cnt "installed card") " to shuffle into the stack")
                                        :player :corp
                                        :choices {:req #(and (installed? %)
                                                             (= (:side %) "Runner"))
                                                  :max cnt}
                                        :msg (msg "shuffle " (join ", " (map :title targets)) " into the stack")
                                        :effect (req (doseq [c targets]
                                                       (move state :runner c :deck))
                                                     (shuffle! state :runner :deck))}
                                       card nil)))})})
