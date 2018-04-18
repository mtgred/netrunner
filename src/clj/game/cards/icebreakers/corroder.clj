(in-ns 'game.core)

(def card-definitions-icebreakers-corroder
  {"Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 1 1)]})})
