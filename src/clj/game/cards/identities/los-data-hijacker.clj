(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-los-data-hijacker
  {"Los: Data Hijacker"
   {:events {:rez {:once :per-turn
                   :req (req (ice? target))
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain :runner :credit 2))}}}})