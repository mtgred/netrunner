(in-ns 'game.core)

(def card-definitions-ice-kakugo
  {"Kakugo"
   {:events {:pass-ice {:delayed-completion true
                        :req (req (= target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}})
