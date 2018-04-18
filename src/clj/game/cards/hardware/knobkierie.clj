(in-ns 'game.core)

(def card-definitions-hardware-knobkierie
  {"Knobkierie"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]
    :events {:successful-run
             {:req (req (and (first-event? state :runner :successful-run)
                             (pos? (count-virus-programs state))))
              :optional
              {:prompt "Place a virus counter?"
               :yes-ability
               {:delayed-completion true
                :effect (effect (continue-ability
                                  {:prompt "Select an installed virus program"
                                   :choices {:req #(and (installed? %)
                                                        (has-subtype? % "Virus")
                                                        (is-type? % "Program"))}
                                   :msg (msg "place 1 virus counter on " (:title target))
                                   :effect (effect (add-counter target :virus 1))}
                                  card nil))}}}}}})
