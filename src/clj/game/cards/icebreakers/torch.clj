(in-ns 'game.core)

(def card-definitions-icebreakers-torch
  {"Torch"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})})
