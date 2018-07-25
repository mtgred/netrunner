(in-ns 'game.cards.operations)

(def card-definition-high-profile-target
  {"High-Profile Target"
   (letfn [(dmg-count [runner]
             (* 2 (:tag runner)))]
     {:req (req tagged)
      :async true
      :msg (msg "do " (dmg-count runner) " meat damage")
      :effect (effect (damage eid :meat (dmg-count runner) {:card card}))})})
