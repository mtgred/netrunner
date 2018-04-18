(in-ns 'game.core)

(def card-definitions-ice-gutenberg
  {"Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}})
