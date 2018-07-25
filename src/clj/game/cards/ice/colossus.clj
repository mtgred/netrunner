(in-ns 'game.cards.ice)

(def card-definition-colossus
  {"Colossus"
   {:advanceable :always
    :subroutines [{:label "Give the Runner 1 tag (Give the Runner 2 tags)"
                   :async true
                   :msg (msg "give the Runner " (if (wonder-sub card 3) "2 tags" "1 tag"))
                   :effect (effect (gain-tags :corp eid (if (wonder-sub card 3) 2 1)))}
                  {:label "Trash 1 program (Trash 1 program and 1 resource)"
                   :async true
                   :msg (msg "trash 1 program" (when (wonder-sub card 3) " and 1 resource"))
                   :effect (req (wait-for (resolve-ability state side trash-program card nil)
                                          (if (wonder-sub card 3)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a resource to trash"
                                               :msg (msg "trash " (:title target))
                                               :choices {:req #(and (installed? %)
                                                                    (is-type? % "Resource"))}
                                               :cancel-effect (req (effect-completed state side eid))
                                               :effect (effect (trash target {:cause :subroutine}))}
                                              card nil)
                                            (effect-completed state side eid))))}]
    :strength-bonus advance-counters}})
