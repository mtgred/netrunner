(in-ns 'game.cards.resources)

(def card-definition-first-responders
  {"First Responders"
   {:abilities [{:cost [:credit 2]
                 :req (req (some #(= (:side %) "Corp") (map second (turn-events state :runner :damage))))
                 :msg "draw 1 card"
                 :effect (effect (draw))}]}})
