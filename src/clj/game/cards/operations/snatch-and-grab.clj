(in-ns 'game.core)

(def card-definitions-operations-snatch-and-grab
  {"Snatch and Grab"
   {:trace {:msg "trash a connection"
            :base 3
            :choices {:req #(has-subtype? % "Connection")}
            :delayed-completion true
            :effect (req (let [c target]
                           (show-wait-prompt state :corp "Runner to decide if they will take 1 tag")
                           (continue-ability
                             state side
                             {:prompt (msg "Take 1 tag to prevent " (:title c) " from being trashed?")
                              :choices ["Yes" "No"] :player :runner
                              :delayed-completion true
                              :effect (effect (clear-wait-prompt :corp)
                                              (continue-ability
                                                (if (= target "Yes")
                                                  {:msg (msg "take 1 tag to prevent " (:title c)
                                                             " from being trashed")
                                                   :delayed-completion true
                                                   :effect (effect (tag-runner eid 1 {:unpreventable true}))}
                                                  {:delayed-completion true
                                                   :effect (effect (trash :corp eid c nil))
                                                   :msg (msg "trash " (:title c))})
                                                card nil))}
                             card nil)))}}})
