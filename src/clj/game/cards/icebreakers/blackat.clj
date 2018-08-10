(in-ns 'game.cards.icebreakers)

(def card-definition-blackat
  {"BlacKat"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 1]
                 :msg "break up to 3 Barrier subroutines (using a stealth [Credits])"}
                (strength-pump 2 1)
                {:cost [:credit 2]
                 :msg "add 2 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 2)) :pump 2}]}})
