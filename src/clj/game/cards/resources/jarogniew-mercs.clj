(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-jarogniew-mercs
  {"Jarogniew Mercs"
   {:effect (effect (tag-runner :runner eid 1)
                    (add-counter card :power (-> @state :runner :tag (+ 3))))
    :flags {:untrashable-while-resources true}
    :prevent {:damage [:meat]}
    :abilities [{:label "Prevent 1 meat damage"
                 :counter-cost [:power 1]
                 :effect (req (damage-prevent state side :meat 1)
                              (when (<= (get-in card [:counter :power]) 0)
                                (trash state :runner card {:unpreventable true})))}]}})
