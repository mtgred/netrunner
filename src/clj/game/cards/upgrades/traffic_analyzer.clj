(in-ns 'game.cards.upgrades)

(def card-definition-traffic-analyzer
  {"Traffic Analyzer"
   {:events {:rez {:req (req (and (protecting-same-server? card target)
                                  (ice? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :successful {:msg "gain 1 [Credits]"
                                        :effect (effect (gain-credits 1))}}}}}})
