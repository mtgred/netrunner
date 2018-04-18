(in-ns 'game.core)

(def card-definitions-assets-bio-ethics-association
  {"Bio-Ethics Association"
   (let [ability {:req (req unprotected)
                  :delayed-completion true
                  :label "Do 1 net damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 net damage"
                  :effect (effect (damage eid :net 1 {:card card}))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})})
