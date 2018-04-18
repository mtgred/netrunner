(in-ns 'game.core)

(def card-definitions-icebreakers-black-orchestra
  {"Black Orchestra"
   (conspiracy "Black Orchestra" "Code Gate"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])})
