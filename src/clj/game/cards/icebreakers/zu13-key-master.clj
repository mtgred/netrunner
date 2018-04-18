(in-ns 'game.core)

(def card-definitions-icebreakers-zu13-key-master
  {"ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "Code Gate")
                                   (strength-pump 1 1)]}))})
