(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-mr-stone
  {"Mr. Stone"
   {:events {:runner-gain-tag {:delayed-completion true
                               :msg "do 1 meat damage"
                               :effect (effect (damage :corp eid :meat 1 {:card card}))}}}})