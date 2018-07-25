(in-ns 'game.cards.operations)

(def card-definition-punitive-counterstrike
  {"Punitive Counterstrike"
   {:trace {:base 5
            :successful {:async true
                         :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
                         :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}}})
