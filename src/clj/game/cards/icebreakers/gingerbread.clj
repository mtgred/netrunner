(in-ns 'game.cards.icebreakers)

(def card-definition-gingerbread
  {"Gingerbread"
   (auto-icebreaker ["Tracer"]
                    {:abilities [(break-sub 1 1 "Tracer")
                                 (strength-pump 2 3)]})})
