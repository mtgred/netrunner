(in-ns 'game.core)

(def card-hardware-turntable
  {"Turntable"
   {:in-play [:memory 1]
    :events {:agenda-stolen
             {:interactive (req true)
              :req (req (not (empty? (:scored corp))))
              :delayed-completion true
              :effect (req
                        (let [stolen target]
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                              :yes-ability
                              {:delayed-completion true
                               :effect (req
                                         (continue-ability
                                           state side
                                           {:prompt (str "Select a scored Corp agenda to swap with " (:title stolen))
                                            :choices {:req #(in-corp-scored? state side %)}
                                            :effect (req (let [scored target]
                                                           (swap-agendas state side scored stolen)
                                                           (system-msg state side (str "uses Turntable to swap "
                                                                                       (:title stolen) " for " (:title scored)))
                                                           (effect-completed state side eid card)))}
                                           card targets))}}}
                            card targets)))}}}})
