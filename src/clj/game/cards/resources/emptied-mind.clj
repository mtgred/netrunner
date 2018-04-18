(in-ns 'game.core)

(def card-definitions-resources-emptied-mind
  {"Emptied Mind"
   (let [ability {:req (req (= 0 (count (:hand runner))))
                  :msg "gain [Click]"
                  :label "Gain [Click] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :click 1))}]
     {:events {:runner-turn-begins ability}
      :abilities [ability]})})
