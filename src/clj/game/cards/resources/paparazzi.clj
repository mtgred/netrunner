(in-ns 'game.core)

(def card-definitions-resources-paparazzi
  {"Paparazzi"
   {:effect (req (swap! state update-in [:runner :tagged] inc))
    :events {:pre-damage {:req (req (= target :meat)) :msg "prevent all meat damage"
                          :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}}
    :leave-play (req (swap! state update-in [:runner :tagged] dec))}})
