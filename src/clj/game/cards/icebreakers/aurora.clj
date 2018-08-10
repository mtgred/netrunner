(in-ns 'game.cards.icebreakers)

(def card-definition-aurora
  {"Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 2 3)]})})
