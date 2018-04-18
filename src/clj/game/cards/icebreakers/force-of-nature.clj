(in-ns 'game.core)

(def card-definitions-icebreakers-force-of-nature
  {"Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})})
