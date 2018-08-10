(in-ns 'game.cards.ice)

(def card-definition-meru-mati
  {"Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}})
