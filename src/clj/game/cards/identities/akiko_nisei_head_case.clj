(in-ns 'game.cards.identities)

(def card-definition-akiko-nisei-head-case
  {"Akiko Nisei: Head Case"
   {:events {:pre-access {:req (req (= target :rd))
                          :interactive (req true)
                          :psi {:player :runner
                                :equal {:msg "access 1 additional card"
                                        :effect (effect (access-bonus 1)
                                                        (effect-completed eid))}}}}}})
