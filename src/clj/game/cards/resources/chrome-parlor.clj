(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-chrome-parlor
  {"Chrome Parlor"
   {:events
    {:pre-damage {:req (req (has-subtype? (second targets) "Cybernetic"))
                  :effect (effect (damage-prevent target Integer/MAX_VALUE))}}}})