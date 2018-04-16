(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-traffic-analyzer
  {"Traffic Analyzer"
   {:events {:rez {:req (req (and (protecting-same-server? card target)
                                  (ice? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :msg "gain 1 [Credits]"
                           :effect (effect (gain :credit 1))}}}}})