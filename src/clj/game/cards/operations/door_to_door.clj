(in-ns 'game.cards.operations)

(def card-definition-door-to-door
  {"Door to Door"
   {:events {:runner-turn-begins
             {:trace {:base 1
                      :label "Do 1 meat damage if Runner is tagged, or give the Runner 1 tag"
                      :successful {:msg (msg (if tagged
                                               "do 1 meat damage"
                                               "give the Runner 1 tag"))
                                   :async true
                                   :effect (req (if tagged
                                                  (damage state side eid :meat 1 {:card card})
                                                  (gain-tags state :corp eid 1)))}}}}}})
