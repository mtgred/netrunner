(in-ns 'game.core)

(def card-definitions-hardware-maui
  {"Māui"
   {:in-play [:memory 2]
    :recurring (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))
    :effect (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))}})
