(in-ns 'game.core)

(def card-definitions-agendas-labyrinthine-servers
  {"Labyrinthine Servers"
   {:prevent {:jack-out [:all]}
    :silent (req true)
    :effect (effect (add-counter card :power 2))
    :abilities [{:req (req (:run @state))
                 :counter-cost [:power 1]
                 :effect (req (let [ls (filter #(= "Labyrinthine Servers" (:title %)) (:scored corp))]
                                (jack-out-prevent state side)
                                (when (zero? (reduce + (for [c ls] (get-in c [:counter :power]))))
                                  (swap! state update-in [:prevent] dissoc :jack-out))))
                 :msg "prevent the Runner from jacking out"}]}})
