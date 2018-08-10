(in-ns 'game.cards.ice)

(def card-definition-kakugo
  {"Kakugo"
   {:events {:pass-ice {:async true
                        :req (req (= target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}})
