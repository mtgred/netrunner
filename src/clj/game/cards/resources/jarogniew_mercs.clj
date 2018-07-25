(in-ns 'game.cards.resources)

(def card-definition-jarogniew-mercs
  {"Jarogniew Mercs"
   {:effect (effect (gain-tags :runner eid 1)
                    (add-counter card :power (-> @state :runner :tag (+ 3))))
    :flags {:untrashable-while-resources true}
    :interactions {:prevent [{:type #{:meat}
                              :req (req true)}]}
    :abilities [{:label "Prevent 1 meat damage"
                 :counter-cost [:power 1]
                 :effect (req (damage-prevent state side :meat 1)
                              (when (zero? (get-counters (get-card state card) :power))
                                (trash state :runner card {:unpreventable true})))}]}})
