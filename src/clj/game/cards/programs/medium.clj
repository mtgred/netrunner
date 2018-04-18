(in-ns 'game.core)

(def card-definitions-programs-medium
  {"Medium"
   {:events
    {:successful-run {:req (req (= target :rd))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:delayed-completion true
                  :req (req (= target :rd))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state side card)))
                                     :prompt "Choose how many additional R&D accesses to make with Medium"
                                     :choices {:number (req (dec (get-virus-counters state side card)))
                                               :default (req (dec (get-virus-counters state side card)))}
                                     :msg (msg "access " target " additional cards from R&D")
                                     :effect (effect (access-bonus (max 0 target)))}
                                    card nil))}}}
   "Misdirection"
   {:abilities [{:cost [:click 2]
                 :prompt "How many [Credits] to spend to remove that number of tags?"
                 :choices {:number (req (min (:credit runner) (:tag runner)))}
                 :msg (msg "spend " target " [Credits] and remove " target " tags")
                 :effect (effect (lose :credit target)
                                 (lose :tag target))}]}})
