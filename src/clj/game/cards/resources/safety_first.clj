(in-ns 'game.cards.resources)

(def card-definition-safety-first
  {"Safety First"
   {:in-play [:hand-size {:mod -2}]
    :events {:runner-turn-ends
             {:async true
              :effect (req (if (< (count (:hand runner)) (hand-size state :runner))
                             (do (system-msg state :runner (str "uses " (:title card) " to draw a card"))
                                 (draw state :runner eid 1 nil))
                             (effect-completed state :runner eid)))}}}})
