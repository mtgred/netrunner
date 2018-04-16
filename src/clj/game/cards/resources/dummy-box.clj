(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-dummy-box
  {"Dummy Box"
   (letfn [(dummy-prevent [type] {:msg (str "prevent a " type " from being trashed")
                                  :delayed-completion true
                                  :priority 15
                                  :prompt (str "Choose a " type " in your Grip")
                                  :choices {:req #(and (is-type? % (capitalize type))
                                                       (in-hand? %))}
                                  :effect (effect (move target :discard)
                                                  (trash-prevent (keyword type) 1))})]
     {:prevent {:trash [:hardware :resource :program]}
      :abilities [(dummy-prevent "hardware")
                  (dummy-prevent "resource")
                  (dummy-prevent "program")]})})
