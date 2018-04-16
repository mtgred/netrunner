(in-ns 'game.core)

(def card-operations-sea-source
  {"SEA Source"
   {:req (req (last-turn? state :runner :successful-run))
    :trace {:base 3
            :msg "give the Runner 1 tag"
            :label "Give the Runner 1 tag"
            :delayed-completion true
            :effect (effect (tag-runner :runner eid 1))}}})