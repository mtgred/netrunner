(in-ns 'game.cards.agendas)

(def card-definition-labyrinthine-servers
  {"Labyrinthine Servers"
   {:interactions {:prevent [{:type #{:jack-out}
                              :req (req (pos? (get-counters card :power)))}]}
    :silent (req true)
    :effect (effect (add-counter card :power 2))
    :abilities [{:req (req (:run @state))
                 :counter-cost [:power 1]
                 :effect (req (let [ls (filter #(= "Labyrinthine Servers" (:title %)) (:scored corp))]
                                (jack-out-prevent state side)
                                (when (zero? (reduce + (for [c ls] (get-counters c :power))))
                                  (swap! state update-in [:prevent] dissoc :jack-out))))
                 :msg "prevent the Runner from jacking out"}]}})
