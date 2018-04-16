(in-ns 'game.core)

(declare can-host?)

(def card-programs-trope
  {"Trope"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :label "[Click], remove Trope from the game: Reshuffle cards from Heap back into Stack"
                 :effect (effect
                          (move card :rfg)
                          (gain :memory 1)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max (min (get-in card [:counter :power] 0) (count (:discard runner)))
                                      :all true
                                      :req #(and (= (:side %) "Runner")
                                                 (in-discard? %))}
                            :msg (msg "shuffle " (join ", " (map :title targets))
                                      " into their Stack")
                            :effect (req (doseq [c targets] (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}
     "Upya"
     {:implementation "Power counters added automatically"
      :events {:successful-run {:silent (req true)
                                :req (req (= target :rd))
                                :effect (effect (add-counter card :power 1)) }}
      :abilities [{:cost [:click 1]
                   :counter-cost [:power 3]
                   :once :per-turn
                   :msg "gain [Click][Click]"
                   :effect (effect (gain :click 2))}]}})
