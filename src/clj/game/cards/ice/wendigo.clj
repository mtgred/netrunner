(in-ns 'game.cards.ice)

(def card-definition-wendigo
  {"Wendigo"
   (implementation-note
     "Program prevention is not implemented"
     (morph-ice "Code Gate" "Barrier"
                {:msg "prevent the Runner from using a chosen program for the remainder of this run"}))})
