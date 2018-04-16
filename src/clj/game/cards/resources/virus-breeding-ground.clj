(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-virus-breeding-ground
  {"Virus Breeding Ground"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move 1 virus counter to " (:title target))
                 :req (req (pos? (get-in card [:counter :virus] 0)))
                 :choices {:req #(and (has-subtype? % "Virus")
                                      (pos? (get-in % [:counter :virus] 0)))}
                 :effect (req (when (pos? (get-virus-counters state side target))
                                (add-counter state side card :virus -1)
                                (add-counter state side target :virus 1)))}]}})
