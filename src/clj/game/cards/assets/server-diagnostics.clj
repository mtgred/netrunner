(in-ns 'game.core)

(def card-definitions-assets-server-diagnostics
  {"Server Diagnostics"
   (let [ability {:effect (effect (gain :credit 2))
                  :once :per-turn
                  :label "Gain 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [ability]
    :events {:corp-turn-begins ability
             :corp-install {:req (req (ice? target))
                            :effect (effect (trash card)
                                            (system-msg "trashes Server Diagnostics"))}}})})
