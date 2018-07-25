(in-ns 'game.cards.events)

(def card-definition-another-day-another-paycheck
  {"Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :unsuccessful
                      {:effect (effect (gain-credits
                                         :runner (+ (:agenda-point runner) (:agenda-point corp))))
                       :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}}}})
