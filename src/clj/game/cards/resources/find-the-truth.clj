(in-ns 'game.core)

(def card-definitions-resources-find-the-truth
  {"Find the Truth"
   {:events {:post-runner-draw {:msg (msg "reveal that they drew: "
                                          (join ", " (map :title (get-in @state [:runner :register :most-recent-drawn]))))}
             :pre-successful-run {:interactive (req true)
                                  :optional {:delayed-completion true
                                             :req (req (= 1 (count (get-in @state [:runner :register :successful-run]))))
                                             :prompt "Use Find the Truth to look at the top card of R&D?"
                                             :yes-ability {:msg "look at the top card of R&D"
                                                           :effect (req (prompt! state :runner card (str "The top card of R&D is "
                                                                                                         (:title (first (:deck corp)))) ["OK"] {})
                                                                        (effect-completed state side eid))}
                                             :no-ability {:effect (req (effect-completed state side eid))}}}}}})
