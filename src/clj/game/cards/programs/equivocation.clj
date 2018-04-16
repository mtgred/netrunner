(in-ns 'game.core)

(declare can-host?)

(def card-programs-equivocation
  {"Equivocation"
   (let [force-draw (fn [title]
                      {:optional {:prompt (str "Force the Corp to draw " title "?")
                                  :yes-ability {:delayed-completion true
                                                :effect (req (show-wait-prompt state :runner "Corp to draw")
                                                             (when-completed (draw state :corp 1 nil)
                                                                             (do (system-msg state :corp (str "is forced to draw " title))
                                                                                 (clear-wait-prompt state :runner)
                                                                                 (effect-completed state side eid))))}}})
         reveal {:optional {:prompt "Reveal the top card of R&D?"
                            :yes-ability {:delayed-completion true
                                          :effect (req (let [topcard (-> corp :deck first :title)]
                                                         (system-msg state :runner (str "reveals " topcard
                                                                                        " from the top of R&D"))
                                                         (continue-ability state side (force-draw topcard) card nil)))}}}]
     {:events {:successful-run {:req (req (= target :rd))
                                :delayed-completion true
                                :interactive (req true)
                                :effect (effect (continue-ability reveal card nil))}}})})