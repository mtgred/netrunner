(in-ns 'game.cards.operations)

(def card-definition-sea-source
  {"SEA Source"
   {:req (req (last-turn? state :runner :successful-run))
    :trace {:base 3
            :label "Trace 3 - Give the Runner 1 tag"
            :successful {:msg "give the Runner 1 tag"
                         :async true
                         :effect (effect (gain-tags :corp eid 1))}}}})
