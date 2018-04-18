(in-ns 'game.core)

(def card-definitions-icebreakers-study-guide
  {"Study Guide"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2] :msg "place 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (get-in card [:counter :power] 0))}})
