(in-ns 'game.cards.resources)

(def card-definition-london-library
  {"London Library"
   {:abilities [{:label "Install a non-virus program on London Library"
                 :cost [:click 1]
                 :prompt "Select a non-virus program to install on London Library from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Virus"))
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (runner-install target {:host-card card :no-cost true}))}
                {:label "Add a program hosted on London Library to your Grip"
                 :cost [:click 1]
                 :choices {:req #(:host %)} ;TODO: this seems to allow all hosted cards to be bounced
                 :msg (msg "add " (:title target) " to their Grip")
                 :effect (effect (move target :hand))}]
    :events {:runner-turn-ends {:effect (req (doseq [c (:hosted card)]
                                               (when (is-type? c "Program")
                                                 (trash state side c))))}}}})
