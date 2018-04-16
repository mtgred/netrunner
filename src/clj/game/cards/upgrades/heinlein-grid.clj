(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-heinlein-grid
  {"Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]") :once :per-run
                 :effect (effect (lose :runner :credit :all :run-credit :all))}]}})
