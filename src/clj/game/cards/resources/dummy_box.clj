(in-ns 'game.cards.resources)

(def card-definition-dummy-box
  {"Dummy Box"
   (letfn [(dummy-prevent [type] {:msg (str "prevent a " type " from being trashed")
                                  :async true
                                  :priority 15
                                  :prompt (str "Choose a " type " in your Grip")
                                  :choices {:req #(and (is-type? % (capitalize type))
                                                       (in-hand? %))}
                                  :effect (effect (move target :discard)
                                                  (trash-prevent (keyword type) 1))})]
     {:interactions {:prevent [{:type #{:trash-hardware :trash-resource :trash-program}
                                :req (req (not= :purge (:cause target)))}]}
      :abilities [(dummy-prevent "hardware")
                  (dummy-prevent "resource")
                  (dummy-prevent "program")]})})
