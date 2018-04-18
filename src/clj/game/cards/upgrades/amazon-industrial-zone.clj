(in-ns 'game.core)

(def card-definitions-upgrades-amazon-industrial-zone
  {"Amazon Industrial Zone"
   {:events
     {:corp-install  {:optional {:req (req (and (ice? target)
                                                (protecting-same-server? card target)))
                                 :prompt "Rez ICE with rez cost lowered by 3?" :priority 2
                                 :yes-ability {:effect (effect (rez-cost-bonus -3) (rez target))}}}}}})
