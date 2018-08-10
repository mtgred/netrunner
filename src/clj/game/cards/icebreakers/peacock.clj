(in-ns 'game.cards.icebreakers)

(def card-definition-peacock
  {"Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 2 3)]})})
