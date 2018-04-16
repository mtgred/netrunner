(in-ns 'game.core)

(def card-operations-boom!
  {"BOOM!"
   {:req (req (> (:tag runner) 1))
    :delayed-completion true
    :msg "do 7 meat damage"
    :effect (effect (damage eid :meat 7 {:card card}))}})