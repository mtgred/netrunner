(in-ns 'game.core)

(def card-definitions-upgrades-traffic-analyzer
  {"Traffic Analyzer"
   {:events {:rez {:req (req (and (protecting-same-server? card target)
                                  (ice? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :msg "gain 1 [Credits]"
                           :effect (effect (gain :credit 1))}}}}})
