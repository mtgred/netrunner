(in-ns 'game.cards.resources)

(def card-definition-citadel-sanctuary
  {"Citadel Sanctuary"
   {:interactions {:prevent [{:type #{:meat}
                              :req (req true)}]}
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
              :trace {:base 1
                      :unsuccessful {:msg "remove 1 tag"
                                     :async true
                                     :effect (effect (lose-tags :runner eid 1))}}}}}})
