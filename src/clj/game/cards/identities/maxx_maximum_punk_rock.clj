(in-ns 'game.cards.identities)

(def card-definition-maxx-maximum-punk-rock
  {"MaxX: Maximum Punk Rock"
   (let [ability {:label "Trash and draw"
                  :msg (msg (let [deck (:deck runner)]
                              (if (pos? (count deck))
                                (str "trash " (join ", " (map :title (take 2 deck)))
                                     " from their Stack and draw 1 card")
                                "trash the top 2 cards from their Stack and draw 1 card - but their Stack is empty")))
                  :once :per-turn
                  :effect (effect (mill :runner 2)
                                  (draw))}]
     {:flags {:runner-turn-draw true
              :runner-phase-12 (req (and (not (:disabled card))
                                         (some #(card-flag? % :runner-turn-draw true)
                                               (all-active-installed state :runner))))}
      :events {:runner-turn-begins ability}
      :abilities [ability]})})
