(in-ns 'game.cards.identities)

(def card-definition-sportsmetal-go-big-or-go-home
  {"Sportsmetal: Go Big or Go Home"
   (let [ab {:prompt "Gain 2 credits or draw 2 cards?"
             :player :corp
             :choices ["2 credits" "2 cards"]
             :msg "gain 2 [Credits] or draw 2 cards"
             :async true
             :interactive (req true)
             :effect (req (if (= target "2 credits")
                            (do (system-msg state side "chooses to take 2 [Credits]")
                                (gain-credits state :corp 2)
                                (effect-completed state side eid))
                            (do (system-msg state side "chooses to draw 2 cards")
                                (draw state :corp eid 2 nil))))}]
     {:events {:agenda-scored ab
               :agenda-stolen ab}})})
