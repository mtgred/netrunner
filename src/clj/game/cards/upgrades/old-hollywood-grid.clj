(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-old-hollywood-grid
  {"Old Hollywood Grid"
   (let [ohg {:req (req (or (in-same-server? card target)
                            (from-same-server? card target)))
              :effect (effect (register-persistent-flag!
                                card :can-steal
                                (fn [state _ card]
                                  (if-not (some #(= (:title %) (:title card)) (:scored runner))
                                    ((constantly false)
                                      (toast state :runner "Cannot steal due to Old Hollywood Grid." "warning"))
                                    true))))}]
     {:trash-effect
              {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
               :effect (effect (register-events {:pre-steal-cost (assoc ohg :req (req (or (= (:zone (get-nested-host target)) (:previous-zone card))
                                                                                          (= (central->zone (:zone target))
                                                                                             (butlast (:previous-zone card))))))
                                                 :run-ends {:effect (effect (unregister-events card))}}
                                                (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ohg
               :post-access-card {:effect (effect (clear-persistent-flag! target :can-steal))}}})})