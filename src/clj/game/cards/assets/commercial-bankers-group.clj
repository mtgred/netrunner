(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-commercial-bankers-group
  {"Commercial Bankers Group"
   (let [ability {:req (req unprotected)
                  :label "Gain 3 [Credits] (start of turn)"
                  :once :per-turn
                  :msg "gain 3 [Credits]"
                  :effect (effect (gain :credit 3))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})})
