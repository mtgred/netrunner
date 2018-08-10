(in-ns 'game.cards.upgrades)

(def card-definition-intake
  {"Intake"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 4
                     :label "add an installed program or virtual resource to the Grip"
                     :successful
                     {:async true
                      :effect (effect (show-wait-prompt :runner "Corp to resolve Intake")
                                      (continue-ability
                                        {:prompt "Select a program or virtual resource"
                                         :player :corp
                                         :choices {:req #(and (installed? %)
                                                              (or (program? %)
                                                                  (and (resource? %)
                                                                       (has-subtype? % "Virtual"))))}
                                         :msg (msg "move " (:title target) " to the Grip")
                                         :effect (effect (move :runner target :hand))
                                         :end-effect (req (clear-wait-prompt state :runner)
                                                          (effect-completed state side eid))}
                                        card nil))}}}}})
