(in-ns 'game.cards.ice)

(def card-definition-bandwidth
  {"Bandwidth"
   {:subroutines [{:msg "give the Runner 1 tag"
                   :async true
                   :effect (effect (gain-tags :corp eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose-tags :corp 1))
                                                       :msg "make the Runner lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}})
