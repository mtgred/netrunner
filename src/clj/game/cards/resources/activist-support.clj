(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-activist-support
  {"Activist Support"
   {:events
    {:corp-turn-begins {:req (req (= 0 (:tag runner)))
                        :msg "take 1 tag"
                        :delayed-completion true
                        :effect (effect (tag-runner :runner eid 1))}
     :runner-turn-begins {:req (req (not has-bad-pub))
                          :msg "give the Corp 1 bad publicity"
                          :effect (effect (gain-bad-publicity :corp 1))}}}})
