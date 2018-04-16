(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-bloodletter
  {"Bloodletter"
   {:subroutines [{:label "Runner trashes 1 program or top 2 cards of their Stack"
                   :effect (req (if (empty? (filter #(is-type? % "Program") (all-active-installed state :runner)))
                                   (do (mill state :runner 2)
                                       (system-msg state :runner (str "trashes the top 2 cards of their Stack")))
                                   (do (show-wait-prompt state :corp "Runner to choose an option for Bloodletter")
                                       (resolve-ability state :runner
                                         {:prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                          :choices ["Trash 1 program" "Trash top 2 of Stack"]
                                          :effect (req (if (and (= target "Trash top 2 of Stack") (pos? (count (:deck runner))))
                                                         (do (mill state :runner 2)
                                                             (system-msg state :runner (str "trashes the top 2 cards of their Stack"))
                                                             (clear-wait-prompt state :corp))
                                                         (resolve-ability state :runner trash-program card nil)))}
                                        card nil))))}]}})