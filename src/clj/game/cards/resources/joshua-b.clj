(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-joshua-b
  {"Joshua B."
   (let [ability {:msg "gain [Click]"
                  :once :per-turn
                  :label "Gain [Click] (start of turn)"
                  :effect (effect (gain :click 1))
                  :end-turn {:delayed-completion true
                             :effect (effect (tag-runner eid 1))
                             :msg "gain 1 tag"}}]
     {:flags {:runner-phase-12 (req true)}
      :events {:runner-turn-begins
               {:optional {:prompt "Use Joshua B. to gain [Click]?"
                           :once :per-turn
                           :yes-ability ability}}}
      :abilities [ability]})})