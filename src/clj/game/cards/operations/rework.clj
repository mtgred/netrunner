(in-ns 'game.core)

(def card-operations-rework
  {"Rework"
   {:prompt "Select a card from HQ to shuffle into R&D"
    :choices {:req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg "shuffle a card from HQ into R&D"
    :effect (final-effect (move target :deck) (shuffle! :deck))}})