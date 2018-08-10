(in-ns 'game.cards.assets)

(def card-definition-urban-renewal
  {"Urban Renewal"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:async true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (wait-for (trash state side card nil)
                                       (do (system-msg state :corp "uses Urban Renewal to do 4 meat damage")
                                           (damage state side eid :meat 4 {:card card})))
                             (effect-completed state side eid)))}}}})
