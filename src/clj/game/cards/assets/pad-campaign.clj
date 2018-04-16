(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-pad-campaign
  {"PAD Campaign"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})})