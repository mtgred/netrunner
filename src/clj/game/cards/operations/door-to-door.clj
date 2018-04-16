(in-ns 'game.core)

(def card-operations-door-to-door
  {"Door to Door"
   {:events {:runner-turn-begins
             {:trace {:base 1 :msg (msg (if tagged "do 1 meat damage" "give the Runner 1 tag"))
                      :label "Do 1 meat damage if Runner is tagged, or give the Runner 1 tag"
                      :delayed-completion true
                      :effect (req (if tagged
                                     (damage state side eid :meat 1 {:card card})
                                     (tag-runner state :runner eid 1)))}}}}})
