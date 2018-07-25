(in-ns 'game.cards.assets)

(def card-definition-pad-campaign
  {"PAD Campaign"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain-credits 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})})
