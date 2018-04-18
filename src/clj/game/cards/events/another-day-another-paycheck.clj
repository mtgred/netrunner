(in-ns 'game.core)

(def card-definitions-events-another-day-another-paycheck
  {"Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :unsuccessful {:effect (effect (gain :runner :credit
                                                           (+ (:agenda-point runner) (:agenda-point corp))))
                                     :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}}}})
