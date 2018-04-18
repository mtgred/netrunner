(in-ns 'game.core)

(def card-definitions-icebreakers-battering-ram
  {"Battering Ram"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 1 1 :all-run)]})})
