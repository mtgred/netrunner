(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-paparazzi
  {"Paparazzi"
   {:effect (req (swap! state update-in [:runner :tagged] inc))
    :events {:pre-damage {:req (req (= target :meat)) :msg "prevent all meat damage"
                          :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}}
    :leave-play (req (swap! state update-in [:runner :tagged] dec))}})