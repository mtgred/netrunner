(in-ns 'game.cards.assets)

(def card-definition-fumiko-yamamori
  {"Fumiko Yamamori"
   {:events {:psi-game-done {:req (req (not= (first targets) (second targets)))
                             :async true
                             :msg "do 1 meat damage"
                             :effect (effect (damage eid :meat 1 {:card card}))}}}})
