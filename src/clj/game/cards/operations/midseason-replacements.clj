(in-ns 'game.core)

(def card-definitions-operations-midseason-replacements
  {"Midseason Replacements"
   {:req (req (last-turn? state :runner :stole-agenda))
    :trace {:base 6
            :msg "give the Runner X tags"
            :label "Give the Runner X tags"
            :delayed-completion true
            :effect (effect (system-msg (str "gives the Runner " (- target (second targets)) " tags"))
                            (tag-runner :runner eid (- target (second targets))))}}})
