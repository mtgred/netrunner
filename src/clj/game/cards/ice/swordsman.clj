(in-ns 'game.cards.ice)

(def card-definition-swordsman
  {"Swordsman"
   {:implementation "AI restriction not implemented"
    :subroutines [(do-net-damage 1)
                  {:prompt "Select an AI program to trash"
                   :msg (msg "trash " (:title target))
                   :label "Trash an AI program"
                   :effect (effect (trash target))
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Program")
                                        (has-subtype? % "AI"))}}]}})
