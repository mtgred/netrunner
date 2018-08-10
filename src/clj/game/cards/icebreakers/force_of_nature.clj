(in-ns 'game.cards.icebreakers)

(def card-definition-force-of-nature
  {"Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})})
