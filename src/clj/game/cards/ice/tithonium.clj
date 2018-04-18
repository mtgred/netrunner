(in-ns 'game.core)

(def card-definitions-ice-tithonium
  {"Tithonium"
   {:alternative-cost [:forfeit]
    :implementation "Does not handle UFAQ for Pawn or Blackguard interaction"
    :cannot-host true
    :subroutines [trash-program
                  end-the-run
                  {:label "Trash a resource"
                   :msg (msg "trash " (:title target))
                   :delayed-completion true
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Resource"))}
                   :effect (effect (trash target {:reason :subroutine}))}]}})
