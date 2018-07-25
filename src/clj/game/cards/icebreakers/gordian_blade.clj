(in-ns 'game.cards.icebreakers)

(def card-definition-gordian-blade
  {"Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1 :all-run)]})})
