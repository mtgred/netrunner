(in-ns 'game.core)

(def card-definitions-events-ive-had-worse
  {"Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:when-inactive true
                   :req (req (#{:meat :net} target))
                   :effect (effect (draw :runner 3)) :msg "draw 3 cards"}}})
