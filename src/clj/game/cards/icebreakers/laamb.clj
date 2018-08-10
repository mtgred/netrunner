(in-ns 'game.cards.icebreakers)

(def card-definition-laamb
  {"Laamb"
   (auto-icebreaker
     ["Barrier"]
     {:abilities [(break-sub 2 0 "Barrier")
                  (strength-pump 3 6)
                  (wrestling-breaker 2 "Barrier")]})})
