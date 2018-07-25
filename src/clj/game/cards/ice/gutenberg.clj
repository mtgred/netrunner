(in-ns 'game.cards.ice)

(def card-definition-gutenberg
  {"Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}})
