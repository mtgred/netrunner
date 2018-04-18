(in-ns 'game.core)

(def card-definitions-ice-bandwidth
  {"Bandwidth"
   {:subroutines [{:msg "give the Runner 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-runner :runner eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose :runner :tag 1))
                                                       :msg "make the Runner lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}})
