(in-ns 'game.core)

(def card-definitions-icebreakers-aurora
  {"Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 2 3)]})})
