(in-ns 'game.cards.agendas)

(def card-definition-vulcan-coverup
  {"Vulcan Coverup"
   {:interactive (req true)
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain-bad-publicity :corp 1))}}})
