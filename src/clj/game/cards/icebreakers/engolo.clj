(in-ns 'game.cards.icebreakers)

(def card-definition-engolo
  {"Engolo"
   (auto-icebreaker
     ["Code Gate"]
     {:abilities [(break-sub 1 1 "Code Gate")
                  (strength-pump 2 4)
                  (wrestling-breaker 2 "Code Gate")]})})
