(in-ns 'game.cards.upgrades)

(def card-definition-heinlein-grid
  {"Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
                 :once :per-run
                 :effect (effect (lose-credits :runner :all)
                                 (lose :runner :run-credit :all))}]}})
