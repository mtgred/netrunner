(in-ns 'game.core)

(def card-definitions-icebreakers-inti
  {"Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 1 :all-run)]})})
