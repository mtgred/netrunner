(in-ns 'game.core)

(def card-operations-hard-hitting-news
  {"Hard-Hitting News"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 4
            :delayed-completion true
            :msg "give the Runner 4 tags"
            :label "Give the Runner 4 tags"
            :effect (effect (tag-runner :runner eid 4))}}})