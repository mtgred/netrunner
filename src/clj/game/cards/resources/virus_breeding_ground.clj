(in-ns 'game.cards.resources)

(def card-definition-virus-breeding-ground
  {"Virus Breeding Ground"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1]
                 :req (req (pos? (get-counters card :virus)))
                 :effect (req (resolve-ability
                                state side
                                {:msg (msg "move 1 virus counter to " (:title target))
                                 :choices {:req #(pos? (get-virus-counters state side %))}
                                 :effect (req (add-counter state side card :virus -1)
                                              (add-counter state side target :virus 1))}
                                card nil))}]}})
