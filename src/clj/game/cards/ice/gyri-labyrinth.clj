(in-ns 'game.core)

(def card-definitions-ice-gyri-labyrinth
  {"Gyri Labyrinth"
   {:implementation "Hand size is not restored if trashed or derezzed after firing"
    :subroutines [{:req (req (:run @state))
                   :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                   :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                   :effect (effect (lose :runner :hand-size-modification 2)
                                   (register-events {:corp-turn-begins
                                                     {:msg "increase the Runner's maximum hand size by 2"
                                                      :effect (effect (gain :runner :hand-size-modification 2)
                                                                      (unregister-events card))}} card))}]
    :events {:corp-turn-begins nil}}})
