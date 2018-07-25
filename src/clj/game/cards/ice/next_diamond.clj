(in-ns 'game.cards.ice)

(def card-definition-next-diamond
  {"NEXT Diamond"
   {:rez-cost-bonus (req (- (next-ice-count corp)))
    :subroutines [(do-brain-damage 1)
                  {:prompt "Select a card to trash"
                   :label "Trash 1 installed Runner card"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (= (:side %) "Runner"))}
                   :async true
                   :effect (req (trash state side eid target {:cause :subroutine}))}]}})
