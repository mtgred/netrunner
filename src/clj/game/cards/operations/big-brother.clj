(in-ns 'game.core)

(def card-definitions-operations-big-brother
  {"Big Brother"
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :delayed-completion true
    :effect (effect (tag-runner :runner eid 2))}})
