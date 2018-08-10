(in-ns 'game.cards.assets)

(def card-definition-siu
  {"SIU"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:label "Trace 3 - Give the Runner 1 tag"
                 :req (req (:corp-phase-12 @state))
                 :async true
                 :effect (effect (trash card {:cause :ability-cost})
                                 (resolve-ability
                                  {:trace {:base 3
                                           :label "Trace 3 - Give the Runner 1 tag"
                                           :successful {:msg "give the Runner 1 tag"
                                                        :async true
                                                        :effect (effect (gain-tags :runner eid 1))}}}
                                  card nil))}]}})
