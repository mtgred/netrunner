(in-ns 'game.core)

(def card-definitions-icebreakers-gordian-blade
  {"Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1 :all-run)]})})
