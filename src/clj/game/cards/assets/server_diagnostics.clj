(in-ns 'game.cards.assets)

(def card-definition-server-diagnostics
  {"Server Diagnostics"
   (let [ability {:effect (effect (gain-credits 2))
                  :once :per-turn
                  :label "Gain 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [ability]
    :events {:corp-turn-begins ability
             :corp-install {:req (req (ice? target))
                            :async true
                            :effect (req (wait-for (trash state side card nil)
                                                   (do (system-msg state :runner "trashes Server Diagnostics")
                                                       (effect-completed state side eid))))}}})})
