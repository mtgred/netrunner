(in-ns 'game.cards.hardware)

(def card-definition-heartbeat
  {"Heartbeat"
   {:in-play [:memory 1]
    :interactions {:prevent [{:type #{:net :brain :meat}
                              :req (req true)}]}
    :abilities [{:msg (msg "prevent 1 damage, trashing a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :priority 50
                 :effect (effect (trash target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}})
