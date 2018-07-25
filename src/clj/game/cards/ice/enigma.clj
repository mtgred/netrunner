(in-ns 'game.cards.ice)

(def card-definition-enigma
  {"Enigma"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}
                  end-the-run]}})
