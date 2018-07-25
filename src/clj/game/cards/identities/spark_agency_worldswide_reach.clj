(in-ns 'game.cards.identities)

(def card-definition-spark-agency-worldswide-reach
  {"Spark Agency: Worldswide Reach"
   {:events
    {:rez {:req (req (and (has-subtype? target "Advertisement")
                          (first-event? state :corp :rez #(has-subtype? (first %) "Advertisement"))))
           :effect (effect (lose-credits :runner 1))
           :msg (msg "make the Runner lose 1 [Credits] by rezzing an Advertisement")}}}})
