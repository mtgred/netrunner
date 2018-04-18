(in-ns 'game.core)

(def card-definitions-programs-incubator
  {"Incubator"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move " (get-in card [:counter :virus] 0) " virus counter to " (:title target))
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Virus"))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (add-counter target :virus (get-in card [:counter :virus] 0)))}]}})
