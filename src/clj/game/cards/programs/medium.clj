(in-ns 'game.cards.programs)

(def card-definition-medium
  {"Medium"
   {:events
    {:successful-run {:req (req (= target :rd))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:async true
                  :req (req (= target :rd))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state side card)))
                                     :prompt "Choose how many additional R&D accesses to make with Medium"
                                     :choices {:number (req (dec (get-virus-counters state side card)))
                                               :default (req (dec (get-virus-counters state side card)))}
                                     :msg (msg "access " target " additional cards from R&D")
                                     :effect (effect (access-bonus (max 0 target)))}
                                    card nil))}}}})
