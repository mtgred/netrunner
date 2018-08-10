(in-ns 'game.cards.events)

(def card-definition-guinea-pig
  {"Guinea Pig"
   {:msg "trash all cards in the grip and gain 10 [credits]"
    :effect (req (doseq [c (:hand runner)]
                   (trash state :runner c {:unpreventable true}))
                 (gain-credits state :runner 10))}})
