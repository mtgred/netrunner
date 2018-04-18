(in-ns 'game.core)

(def card-definitions-events-three-steps-ahead
  {"Three Steps Ahead"
   {:end-turn {:effect (effect (gain :credit (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}})
