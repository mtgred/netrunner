(in-ns 'game.cards.resources)

(def card-definition-spoilers
  {"Spoilers"
   {:events {:agenda-scored {:interactive (req true)
                             :msg "trash the top card of R&D"
                             :effect (effect (mill :corp))}}}})
