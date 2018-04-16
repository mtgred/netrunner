(in-ns 'game.core)

(def card-hardware-net-ready-eyes
  {"Net-Ready Eyes"
   {:effect (effect (damage eid :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}})
