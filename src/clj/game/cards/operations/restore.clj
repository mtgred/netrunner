(in-ns 'game.cards.operations)

(def card-definition-restore
  {"Restore"
   {:async true
    :effect (effect (continue-ability {:prompt "Select a card in Archives to install & rez with Restore"
                                       :priority -1
                                       :async true
                                       :show-discard true
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-discard? %))}
                                       :effect (req (wait-for
                                                      (corp-install state side target nil {:install-state :rezzed})
                                                      (do (system-msg state side (str "uses Restore to "
                                                                                      (corp-install-msg target)))
                                                          (let [leftover (filter #(= (:title target) (:title %)) (-> @state :corp :discard))]
                                                            (when (seq leftover)
                                                              (doseq [c leftover]
                                                                (move state side c :rfg))
                                                              (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                                                          (effect-completed state side eid))))}
                                      card nil))}})
