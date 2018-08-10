(in-ns 'game.cards.operations)

(def card-definition-boom
  {"BOOM!"
   {:req (req (> (:tag runner) 1))
    :async true
    :msg "do 7 meat damage"
    :effect (effect (damage eid :meat 7 {:card card}))}})
