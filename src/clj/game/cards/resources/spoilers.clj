(in-ns 'game.core)

(def card-definitions-resources-spoilers
  {"Spoilers"
   {:events {:agenda-scored {:interactive (req true)
                             :msg "trash the top card of R&D" :effect (effect (mill :corp))}}}})
