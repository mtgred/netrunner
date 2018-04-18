(in-ns 'game.core)

(def card-definitions-identities-los-data-hijacker
  {"Los: Data Hijacker"
   {:events {:rez {:once :per-turn
                   :req (req (ice? target))
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain :runner :credit 2))}}}})
