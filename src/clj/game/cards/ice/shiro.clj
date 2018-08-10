(in-ns 'game.cards.ice)

(def card-definition-shiro
  {"Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :async true
                   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck corp))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :runner)
                                        (effect-completed state side eid)))))}
                  {:label "Force the Runner to access the top card of R&D"
                   :async true
                   :effect (req (do-access state :runner eid [:rd] {:no-root true}))}]}})
