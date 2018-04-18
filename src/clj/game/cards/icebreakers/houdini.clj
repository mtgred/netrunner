(in-ns 'game.core)

(def card-definitions-icebreakers-houdini
  {"Houdini"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}})
