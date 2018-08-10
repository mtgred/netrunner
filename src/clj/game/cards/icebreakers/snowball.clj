(in-ns 'game.cards.icebreakers)

(def card-definition-snowball
  {"Snowball"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 Barrier subroutine"
                                  :effect (effect (pump card 1 :all-run))}
                                 (strength-pump 1 1)]})})
