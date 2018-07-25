(in-ns 'game.cards.programs)

(def card-definition-imp
  {"Imp"
   {:flags {:slow-trash (req (pos? (get-counters card :virus)))}
    :data {:counter {:virus 2}}
    :interactions {:trash-ability {:interactive (req true)
                                   :label "[Imp]: Trash card"
                                   :req (req (and (not (get-in @state [:per-turn (:cid card)]))
                                                  (pos? (get-counters card :virus))))
                                   :counter-cost [:virus 1]
                                   :msg (msg "trash " (:title target) " at no cost")
                                   :once :per-turn
                                   :async true
                                   :effect (effect (trash-no-cost eid target))}}}})
