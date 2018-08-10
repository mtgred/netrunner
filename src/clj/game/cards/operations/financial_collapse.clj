(in-ns 'game.cards.operations)

(def card-definition-financial-collapse
  {"Financial Collapse"
   {:async true
    :req (req (and (>= (:credit runner) 6) (seq (filter #(is-type? % "Resource") (all-active-installed state :runner)))))
    :effect (req (let [rcount (count (filter #(is-type? % "Resource") (all-active-installed state :runner)))]
                   (if (pos? rcount)
                     (do (show-wait-prompt state :corp "Runner to trash a resource to prevent Financial Collapse")
                         (continue-ability
                           state side
                           {:prompt (msg "Trash a resource to prevent Financial Collapse?")
                            :choices ["Yes" "No"] :player :runner
                            :async true
                            :effect (effect (continue-ability
                                              (if (= target "Yes")
                                                {:player :runner
                                                 :prompt "Select a resource to trash"
                                                 :choices {:req #(and (is-type? % "Resource") (installed? %))}
                                                 :effect (req (trash state side target {:unpreventable true})
                                                              (system-msg state :runner
                                                                          (str "trashes " (:title target)
                                                                               " to prevent Financial Collapse"))
                                                              (clear-wait-prompt state :corp))}
                                                {:effect (effect (lose-credits :runner (* rcount 2))
                                                                 (clear-wait-prompt :corp))
                                                 :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")})
                                              card nil))} card nil))
                     (continue-ability
                       state side
                       {:effect (effect (lose-credits :runner (* rcount 2)))
                        :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")} card nil))))}})
