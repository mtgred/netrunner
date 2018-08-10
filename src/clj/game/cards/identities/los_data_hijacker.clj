(in-ns 'game.cards.identities)

(def card-definition-los-data-hijacker
  {"Los: Data Hijacker"
   {:events {:rez {:once :per-turn
                   :req (req (ice? target))
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain-credits :runner 2))}}}})
