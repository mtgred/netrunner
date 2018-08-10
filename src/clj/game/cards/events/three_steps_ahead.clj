(in-ns 'game.cards.events)

(def card-definition-three-steps-ahead
  {"Three Steps Ahead"
   {:end-turn {:effect (effect (gain-credits (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}})
