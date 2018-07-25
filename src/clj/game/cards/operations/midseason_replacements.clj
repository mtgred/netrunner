(in-ns 'game.cards.operations)

(def card-definition-midseason-replacements
  {"Midseason Replacements"
   {:req (req (last-turn? state :runner :stole-agenda))
    :trace {:base 6
            :label "Trace 6 - Give the Runner X tags"
            :successful {:msg "give the Runner X tags"
                         :async true
                         :effect (effect (system-msg
                                           (str "gives the Runner " (- target (second targets)) " tags"))
                                         (gain-tags eid (- target (second targets))))}}}})
