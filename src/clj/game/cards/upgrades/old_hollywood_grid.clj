(in-ns 'game.cards.upgrades)

(def card-definition-old-hollywood-grid
  {"Old Hollywood Grid"
   (let [ohg {:req (req (or (in-same-server? card target)
                            (from-same-server? card target)))
              :effect (req (register-persistent-flag!
                             state side
                             card :can-steal
                             (fn [state _ card]
                               (if-not (some #(= (:title %) (:title card)) (:scored runner))
                                 ((constantly false)
                                    (toast state :runner "Cannot steal due to Old Hollywood Grid." "warning"))
                                 true))))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (req (register-events
                      state side
                      {:pre-steal-cost (assoc ohg :req (req (or (= (:zone (get-nested-host target))
                                                                   (:previous-zone card))
                                                                (= (central->zone (:zone target))
                                                                   (butlast (:previous-zone card))))))
                       :run-ends {:effect (req (unregister-events state side (find-latest state card))
                                               (clear-persistent-flag! state side card :can-steal))}}
                      (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ohg
               :access {:effect (req (clear-persistent-flag! state side card :can-steal))}
               :run-ends nil}})})
