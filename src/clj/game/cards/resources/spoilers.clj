(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-spoilers
  {"Spoilers"
   {:events {:agenda-scored {:interactive (req true)
                             :msg "trash the top card of R&D" :effect (effect (mill :corp))}}}})
