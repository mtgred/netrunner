(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-vulcan-coverup
  {"Vulcan Coverup"
   {:interactive (req true)
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain-bad-publicity :corp 1))}}})