(in-ns 'game.core)

(def card-definitions-operations-cerebral-cast
  {"Cerebral Cast"
   {:req (req (last-turn? state :runner :successful-run))
    :psi {:not-equal {:player :runner :prompt "Take 1 tag or 1 brain damage?"
                      :choices ["1 tag" "1 brain damage"] :msg (msg "give the Runner " target)
                      :delayed-completion true
                      :effect (req (if (= target "1 tag")
                                     (tag-runner state side eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}})
