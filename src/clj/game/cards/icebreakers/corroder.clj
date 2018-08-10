(in-ns 'game.cards.icebreakers)

(def card-definition-corroder
  {"Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 1 1)]})})
