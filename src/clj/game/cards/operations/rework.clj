(in-ns 'game.cards.operations)

(def card-definition-rework
  {"Rework"
   {:prompt "Select a card from HQ to shuffle into R&D"
    :choices {:req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg "shuffle a card from HQ into R&D"
    :effect (effect (move target :deck)
                    (shuffle! :deck))}})
