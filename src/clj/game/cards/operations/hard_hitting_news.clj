(in-ns 'game.cards.operations)

(def card-definition-hard-hitting-news
  {"Hard-Hitting News"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 4
            :label "Give the Runner 4 tags"
            :successful {:async true
                         :msg "give the Runner 4 tags"
                         :effect (effect (gain-tags eid 4))}}}})
