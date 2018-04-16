(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-spark-agency-worldswide-reach
  {"Spark Agency: Worldswide Reach"
   {:events
    {:rez {:req (req (and (has-subtype? target "Advertisement")
                          (empty? (filter #(has-subtype? % "Advertisement")
                                          (flatten (turn-events state :corp :rez))))))
           :effect (effect (lose :runner :credit 1))
           :msg (msg "make the Runner lose 1 [Credits] by rezzing an Advertisement")}}}})