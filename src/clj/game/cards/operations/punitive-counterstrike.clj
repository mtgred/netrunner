(in-ns 'game.core)

(def card-operations-punitive-counterstrike
  {"Punitive Counterstrike"
   {:trace {:base 5
            :delayed-completion true
            :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
            :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}})