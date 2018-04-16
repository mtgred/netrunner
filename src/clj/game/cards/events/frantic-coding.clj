(in-ns 'game.core)

(declare run-event)

(def card-events-frantic-coding
  {"Frantic Coding"
   {:delayed-completion true
    :events {:runner-shuffle-deck nil}
    :effect
    (req (let [topten (take 10 (:deck runner))]
           (prompt! state :runner card (str "The top 10 cards of the Stack are "
                                            (join ", " (map :title topten))) ["OK"] {})
           (continue-ability
             state side
             {:prompt "Install a program?"
              :choices (conj (vec (sort-by :title (filter #(and (is-type? % "Program")
                                                                (can-pay? state side nil
                                                                          (modified-install-cost state side % [:credit -5])))
                                                          topten))) "No install")
              :delayed-completion true
              :effect (req (if (not= target "No install")
                             (do (register-events state side
                                                  {:runner-shuffle-deck
                                                   {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                  (assoc card :zone '(:discard)))
                                 (install-cost-bonus state side [:credit -5])
                                 (let [to-trash (remove #(= (:cid %) (:cid target)) topten)]
                                   (when-completed (runner-install state side target nil)
                                                   (let [card (get-card state (assoc card :zone '(:discard)))]
                                                     (if (not (:shuffle-occurred card))
                                                       (do (system-msg state side (str "trashes " (join ", " (map :title to-trash))))
                                                           (doseq [c to-trash] (trash state side c {:unpreventable true}))
                                                           (effect-completed state side eid))
                                                       (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                                           (effect-completed state side eid)))))))
                             (do (doseq [c topten] (trash state side c {:unpreventable true}))
                                 (system-msg state side (str "trashes " (join ", " (map :title topten)))))))} card nil)))}})