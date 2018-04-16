(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-citadel-sanctuary
  {"Citadel Sanctuary"
   {:prevent {:damage [:meat]}
    :abilities [{:label "[Trash] and trash all cards in Grip to prevent all meat damage"
                 :msg "trash all cards in their Grip and prevent all meat damage"
                 :effect (req (trash state side card {:cause :ability-cost})
                              (doseq [c (:hand runner)]
                                (trash state side c {:unpreventable true}))
                              (damage-prevent state side :meat Integer/MAX_VALUE))}]
    :events {:runner-turn-ends
             {:req (req (pos? (:tag runner)))
              :msg "force the Corp to initiate a trace"
              :label "Trace 1 - If unsuccessful, Runner removes 1 tag"
              :trace {:base 1 :unsuccessful {:effect (effect (lose :runner :tag 1))
                                             :msg "remove 1 tag"}}}}}})
