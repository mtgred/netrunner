(in-ns 'game.cards.programs)

(def card-definition-trope
  {"Trope"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :label "[Click], remove Trope from the game: Reshuffle cards from Heap back into Stack"
                 :effect (effect
                          (move card :rfg)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max (min (get-counters card :power) (count (:discard runner)))
                                      :all true
                                      :req #(and (= (:side %) "Runner")
                                                 (in-discard? %))}
                            :msg (msg "shuffle " (join ", " (map :title targets))
                                      " into their Stack")
                            :effect (req (doseq [c targets] (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}})
