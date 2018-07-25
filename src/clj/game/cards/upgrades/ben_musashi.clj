(in-ns 'game.cards.upgrades)

(def card-definition-ben-musashi
  {"Ben Musashi"
   (let [bm {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:net-damage 2]))}]
     {:trash-effect
              {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
               :effect (effect (register-events {:pre-steal-cost (assoc bm :req (req (or (= (:zone target) (:previous-zone card))
                                                                                         (= (central->zone (:zone target))
                                                                                            (butlast (:previous-zone card))))))
                                                 :run-ends {:effect (effect (unregister-events card))}}
                                                (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost bm :run-ends nil}})})
