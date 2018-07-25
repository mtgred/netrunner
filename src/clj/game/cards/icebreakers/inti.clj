(in-ns 'game.cards.icebreakers)

(def card-definition-inti
  {"Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 1 :all-run)]})})
