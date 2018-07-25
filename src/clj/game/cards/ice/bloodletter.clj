(in-ns 'game.cards.ice)

(def card-definition-bloodletter
  {"Bloodletter"
   {:subroutines [{:label "Runner trashes 1 program or top 2 cards of their Stack"
                   :effect (req (if (empty? (filter #(is-type? % "Program") (all-active-installed state :runner)))
                                   (do (mill state :runner 2)
                                       (system-msg state :runner (str "trashes the top 2 cards of their Stack")))
                                   (do (show-wait-prompt state :corp "Runner to choose an option for Bloodletter")
                                       (resolve-ability state :runner
                                         {:prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                          :choices ["Trash 1 program" "Trash top 2 of Stack"]
                                          :effect (req (if (and (= target "Trash top 2 of Stack") (> (count (:deck runner)) 1))
                                                         (do (mill state :runner 2)
                                                             (system-msg state :runner (str "trashes the top 2 cards of their Stack")))
                                                         (resolve-ability state :runner trash-program card nil))
                                                      (clear-wait-prompt state :corp))}
                                        card nil))))}]}})
