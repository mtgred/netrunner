(in-ns 'game.core)

(def card-definitions-ice-meru-mati
  {"Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}})
