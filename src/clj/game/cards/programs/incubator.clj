(in-ns 'game.cards.programs)

(def card-definition-incubator
  {"Incubator"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move " (get-counters card :virus) " virus counter to " (:title target))
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Virus"))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (add-counter target :virus (get-counters card :virus)))}]}})
