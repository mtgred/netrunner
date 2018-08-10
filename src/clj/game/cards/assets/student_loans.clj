(in-ns 'game.cards.assets)

(def card-definition-student-loans
  {"Student Loans"
   {:events {:pre-play-instant
             {:req (req (and (is-type? target "Event") (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
              :effect (effect (system-msg :corp (str "makes the runner pay an extra 2 [Credits] due to Student Loans"))
                              (play-cost-bonus [:credit 2]))}}}})
