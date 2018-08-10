(in-ns 'game.cards.assets)

(def card-definition-public-support
  {"Public Support"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:async true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (do (system-msg state :corp "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                                 (as-agenda state :corp eid (dissoc card :counter) 1))
                             (effect-completed state side eid)))}}}})
