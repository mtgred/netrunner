(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-fumiko-yamamori
  {"Fumiko Yamamori"
   {:events {:psi-game-done {:req (req (not= (first targets) (second targets)))
                             :delayed-completion true
                             :msg "do 1 meat damage"
                             :effect (effect (damage eid :meat 1 {:card card}))}}}})