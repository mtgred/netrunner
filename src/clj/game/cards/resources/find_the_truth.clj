(in-ns 'game.cards.resources)

(def card-definition-find-the-truth
  {"Find the Truth"
   {:events {:post-runner-draw {:msg (msg "reveal that they drew: "
                                          (join ", " (map :title (get-in @state [:runner :register :most-recent-drawn]))))}
             :successful-run {:interactive (req true)
                              :optional {:req (req (and (first-event? state side :successful-run)
                                                        (-> @state :corp :deck count pos?)))
                                         :prompt "Use Find the Truth to look at the top card of R&D?"
                                         :yes-ability {:prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                                                       :msg "look at the top card of R&D"
                                                       :choices ["OK"]}}}}}})
