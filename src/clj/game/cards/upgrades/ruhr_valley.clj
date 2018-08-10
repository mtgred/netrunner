(in-ns 'game.cards.upgrades)

(def card-definition-ruhr-valley
  {"Ruhr Valley"
   {:events {:run {:req (req this-server)
                   :effect (effect (lose :runner :click 1))
                   :msg "force the Runner to spend an additional [Click]"}
             :runner-turn-begins {:req (req (> (:click-per-turn runner) 1))
                                  :effect (req (enable-run-on-server state card (second (:zone card))))}
             :runner-spent-click {:req (req (<= 1 (:click runner)))
                                  :effect (req (prevent-run-on-server state card (second (:zone card))))}
             :leave-play (req (enable-run-on-server state card (second (:zone card))))}}})
