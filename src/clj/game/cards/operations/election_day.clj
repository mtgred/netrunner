(in-ns 'game.cards.operations)

(def card-definition-election-day
  {"Election Day"
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   count
                   pos?))
    :async true
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:corp :hand]))
                    (draw eid 5 nil))}})
