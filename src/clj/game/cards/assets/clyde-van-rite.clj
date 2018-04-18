(in-ns 'game.core)

(def card-definitions-assets-clyde-van-rite
  {"Clyde Van Rite"
   (let [ability {:prompt "Pay 1 [Credits] or trash the top card of the Stack"
                  :once :per-turn
                  :choices ["Pay 1 [Credits]" "Trash top card"]
                  :player :runner :msg "make the Runner pay 1 [Credits] or trash the top card of the Stack"
                  :effect (req (cond
                                 ;; Pay 1 credit scenarios
                                 (or (and (= target "Pay 1 [Credits]")
                                          (pos? (:credit runner)))
                                     (and (= target "Trash top card")
                                          (pos? (:credit runner))
                                          (zero? (count (:deck runner)))))
                                 (do (pay state side card :credit 1)
                                     (system-msg state side "pays 1 [Credits]"))

                                 ;; Trash top card scenarios
                                 (or (and (= target "Trash top card")
                                          (pos? (count (:deck runner))))
                                     (and (= target "Pay 1 [Credits]")
                                          (pos? (count (:deck runner)))
                                          (zero? (:credit runner))))
                                 (do (mill state :runner)
                                     (system-msg state side "trashes the top card of the Stack"))

                                 :else (system-msg state side "cannot pay 1 [Credits] or
                                 trash the top card of the Stack")))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :events {:corp-turn-begins ability}
    :abilities [ability]})})
