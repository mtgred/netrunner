(in-ns 'game.core)

(def card-definitions-events-immolation-script
  {"Immolation Script"
   {:req (req archives-runnable)
    :effect (effect (run :archives nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:pre-access
             {:delayed-completion true
              :req (req (and (= target :archives)
                             ;; don't prompt unless there's at least 1 rezzed ICE matching one in Archives
                             (not-empty (clojure.set/intersection
                                          (into #{} (map :title (filter #(ice? %) (:discard corp))))
                                          (into #{} (map :title (filter #(rezzed? %) (all-installed state :corp))))))))
              :effect (req (continue-ability state side
                             {:delayed-completion true
                              :prompt "Choose a piece of ICE in Archives"
                              :choices (req (filter ice? (:discard corp)))
                              :effect (req (let [icename (:title target)]
                                             (continue-ability state side
                                               {:delayed-completion true
                                                :prompt (msg "Select a rezzed copy of " icename " to trash")
                                                :choices {:req #(and (ice? %)
                                                                     (rezzed? %)
                                                                     (= (:title %) icename))}
                                                :msg (msg "trash " (card-str state target))
                                                :effect (req (trash state :corp target)
                                                             (unregister-events state side card)
                                                             (effect-completed state side eid))} card nil)))}
                            card nil))}}}})
