(in-ns 'game.core)

(def card-definitions-ice-shiro
  {"Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :delayed-completion true
                   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck corp))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :runner)
                                        (effect-completed state side eid card)))))}
                  {:label "Force the Runner to access the top card of R&D"
                   :effect (req (doseq [c (take (get-in @state [:runner :rd-access]) (:deck corp))]
                                  (system-msg state :runner (str "accesses " (:title c)))
                                  (handle-access state side [c])))}]}})
