(in-ns 'game.core)

(def card-definitions-operations-high-profile-target
  {"High-Profile Target"
   (letfn [(dmg-count [runner]
             (* 2 (:tag runner)))]
     {:req (req tagged)
      :delayed-completion true
      :msg (msg "do " (dmg-count runner) " meat damage")
      :effect (effect (damage eid :meat (dmg-count runner) {:card card}))})})
