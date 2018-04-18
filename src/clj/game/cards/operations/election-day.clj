(in-ns 'game.core)

(def card-definitions-operations-election-day
  {"Election Day"
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   (count)
                   (pos?)))
    :delayed-completion true
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:corp :hand]))
                    (draw eid 5 nil))}})
