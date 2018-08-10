(in-ns 'game.cards.icebreakers)

(def card-definition-tycoon
  {"Tycoon"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 2 "Barrier" (effect (update! (assoc-in card [:special :tycoon-used] true))))
                                 (strength-pump 2 3)]
                     :events {:pass-ice {:req (req (get-in card [:special :tycoon-used]))
                                         :msg "give the Corp 2 [Credits]"
                                         :effect (effect (update! (dissoc-in card [:special :tycoon-used]))
                                                         (gain-credits :corp 2))}}})})
