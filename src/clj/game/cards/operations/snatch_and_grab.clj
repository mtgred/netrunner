(in-ns 'game.cards.operations)

(def card-definition-snatch-and-grab
  {"Snatch and Grab"
   {:trace {:base 3
            :successful
            {:msg "trash a connection"
             :choices {:req #(has-subtype? % "Connection")}
             :async true
             :effect (req (let [c target]
                            (show-wait-prompt state :corp "Runner to decide if they will take 1 tag")
                            (continue-ability
                              state side
                              {:player :runner
                               :prompt (msg "Take 1 tag to prevent " (:title c) " from being trashed?")
                               :choices ["Yes" "No"]
                               :async true
                               :effect (effect (clear-wait-prompt :corp)
                                               (continue-ability
                                                 (if (= target "Yes")
                                                   {:msg (msg "take 1 tag to prevent " (:title c)
                                                              " from being trashed")
                                                    :async true
                                                    :effect (effect (gain-tags :runner eid 1 {:unpreventable true}))}
                                                   {:async true
                                                    :effect (effect (trash :corp eid c nil))
                                                    :msg (msg "trash " (:title c))})
                                                 card nil))}
                              card nil)))}}}})
