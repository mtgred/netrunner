(in-ns 'game.core)

(def card-definitions-assets-fumiko-yamamori
  {"Fumiko Yamamori"
   {:events {:psi-game-done {:req (req (not= (first targets) (second targets)))
                             :delayed-completion true
                             :msg "do 1 meat damage"
                             :effect (effect (damage eid :meat 1 {:card card}))}}}})
