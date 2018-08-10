(in-ns 'game.cards.icebreakers)

(def card-definition-mammon
  {"Mammon"
   (auto-icebreaker ["All"]
                    {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
                     :abilities [{:label "X [Credits]: Place X power counters"
                                  :prompt "How many power counters to place on Mammon?" :once :per-turn
                                  :choices {:number (req (:credit runner))}
                                  :req (req (:runner-phase-12 @state))
                                  :effect (effect (lose-credits target)
                                                  (add-counter card :power target))
                                  :msg (msg "place " target " power counters on it")}
                                 {:counter-cost [:power 1]
                                  :label "Hosted power counter: Break ICE subroutine"
                                  :msg "break 1 ICE subroutine"}
                                 (strength-pump 2 2)]
                     :events {:runner-turn-ends {:effect (effect (update! (assoc-in card [:counter :power] 0)))}}})})
