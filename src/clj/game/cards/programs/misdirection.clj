(in-ns 'game.cards.programs)

(def card-definition-misdirection
  {"Misdirection"
   {:abilities [{:cost [:click 2]
                 :label "Remove tags"
                 :prompt "How many [Credits] to spend to remove that number of tags?"
                 :choices {:number (req (min (:credit runner)
                                             (:tag runner)))}
                 :msg (msg "spend " target " [Credits] and remove " target " tags")
                 :effect (effect (lose-credits target)
                                 (lose-tags target))}]}})
