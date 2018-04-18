(in-ns 'game.core)

(def card-definitions-resources-chrome-parlor
  {"Chrome Parlor"
   {:events
    {:pre-damage {:req (req (has-subtype? (second targets) "Cybernetic"))
                  :effect (effect (damage-prevent target Integer/MAX_VALUE))}}}})
