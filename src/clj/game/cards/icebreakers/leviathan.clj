(in-ns 'game.cards.icebreakers)

(def card-definition-leviathan
  {"Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "Code Gate")
                                 (strength-pump 3 5)]})})
