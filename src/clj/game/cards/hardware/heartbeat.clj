(in-ns 'game.core)

(def card-definitions-hardware-heartbeat
  {"Heartbeat"
   {:in-play [:memory 1]
    :prevent {:damage [:meat :net :brain]}
    :abilities [{:msg (msg "prevent 1 damage, trashing a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :priority 50
                 :effect (effect (trash target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}})
