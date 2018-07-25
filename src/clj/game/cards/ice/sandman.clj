(in-ns 'game.cards.ice)

(def card-definition-sandman
  {"Sandman"
   {:subroutines [{:label "Add an installed Runner card to the grip"
                   :req (req (not-empty (all-installed state :runner)))
                   :effect (effect (show-wait-prompt :runner "Corp to select Sandman target")
                                   (resolve-ability {:choices {:req #(and (installed? %)
                                                                           (= (:side %) "Runner"))}
                                                      :msg (msg "to add " (:title target) " to the grip")
                                                      :effect (effect (clear-wait-prompt :runner)
                                                                      (move :runner target :hand true))
                                                      :cancel-effect (effect (clear-wait-prompt :runner))}
                                                     card nil))}]}})
