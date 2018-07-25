(in-ns 'game.cards.assets)

(def card-definition-clyde-van-rite
  {"Clyde Van Rite"
   (let [ability {:req (req (or (pos? (:credit runner))
                                (pos? (count (:deck runner)))))
                  :player :runner
                  :once :per-turn
                  :prompt "Pay 1 [Credits] or trash the top card of the Stack"
                  :choices (req (concat (when (pos? (:credit runner))
                                          ["Pay 1 [Credits]"])
                                        (when (pos? (count (:deck runner)))
                                          ["Trash top card"])))
                  :msg "make the Runner pay 1 [Credits] or trash the top card of the Stack"
                  :effect (req (case target
                                 "Pay 1 [Credits]"
                                 (do (system-msg state side "pays 1 [Credits]")
                                     (pay state side card :credit 1))
                                 "Trash top card"
                                 (do (system-msg state side "trashes the top card of the Stack")
                                     (mill state :runner))))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})})
