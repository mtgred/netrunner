(in-ns 'game.cards.ice)

(def card-definition-enforcer-1-0
  {"Enforcer 1.0"
   {:additional-cost [:forfeit]
    :subroutines [trash-program
                  (do-brain-damage 1)
                  {:label "Trash a console"
                   :prompt "Select a console to trash"
                   :choices {:req #(has-subtype? % "Console")}
                   :msg (msg "trash " (:title target))
                   :effect (effect (trash target))}
                  {:msg "trash all virtual resources"
                   :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-active-installed state :runner))]
                                  (trash state side c)))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
