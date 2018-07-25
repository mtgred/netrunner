(in-ns 'game.cards.icebreakers)

(def card-definition-blackstone
  {"Blackstone"
   {:abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 3]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}})
