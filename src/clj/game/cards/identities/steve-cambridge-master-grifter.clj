(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-steve-cambridge-master-grifter
  {"Steve Cambridge: Master Grifter"
   {:events {:successful-run
             {:req (req (and (= target :hq)
                             (first-successful-run-on-server? state :hq)
                             (if (-> @state :run :run-effect :card)
                               (> (count (:discard runner)) 2)
                               (> (count (:discard runner)) 1))))
              :interactive (req true)
              :delayed-completion true
              :effect (effect (continue-ability
                                {:delayed-completion true
                                 :prompt "Select 2 cards in your Heap"
                                 :show-discard true
                                 :choices {:max 2 :req #(and (in-discard? %)
                                                             (= (:side %) "Runner")
                                                             (not= (-> @state :run :run-effect :card :cid) (:cid %)))}
                                 :cancel-effect (req (effect-completed state side eid))
                                 :effect (req (let [c1 (first targets)
                                                    c2 (second targets)]
                                                (show-wait-prompt state :runner "Corp to choose which card to remove from the game")
                                                (continue-ability state :corp
                                                  {:prompt "Choose which card to remove from the game"
                                                   :player :corp
                                                   :choices [c1 c2]
                                                   :effect (req (if (= target c1)
                                                                  (do (move state :runner c1 :rfg)
                                                                      (move state :runner c2 :hand)
                                                                      (system-msg state :runner (str "uses Steve Cambridge: Master Grifter"
                                                                                                     " to add " (:title c2) " to their Grip."
                                                                                                     " Corp removes " (:title c1) " from the game")))
                                                                  (do (move state :runner c2 :rfg)
                                                                      (move state :runner c1 :hand)
                                                                      (system-msg state :runner (str "uses Steve Cambridge: Master Grifter"
                                                                                                     " to add " (:title c1) " to their Grip."
                                                                                                     " Corp removes " (:title c2) " from the game"))))
                                                                (clear-wait-prompt state :runner)
                                                                (effect-completed state side eid))} card nil)))}
                               card nil))}}}})
