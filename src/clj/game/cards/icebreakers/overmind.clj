(in-ns 'game.cards.icebreakers)

(def card-definition-overmind
  {"Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (add-counter card :power (available-mu state)))
                     :abilities [{:counter-cost [:power 1]
                                  :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})})
