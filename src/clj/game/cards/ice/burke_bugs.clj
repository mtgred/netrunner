(in-ns 'game.cards.ice)

(def card-definition-burke-bugs
  {"Burke Bugs"
   {:subroutines [(trace-ability 0 (assoc trash-program :not-distinct true
                                                        :player :runner
                                                        :msg "force the Runner to trash a program"
                                                        :label "Force the Runner to trash a program"))]}})
