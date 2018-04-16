(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-bacterial-programming
  {"Bacterial Programming"
   (letfn [(hq-step [remaining to-trash to-hq]
             {:delayed-completion true
              :prompt "Select a card to move to HQ"
              :choices (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (do
                               (doseq [t to-trash]
                                 (trash state :corp t {:unpreventable true}))
                               (doseq [h to-hq]
                                 (move state :corp h :hand))
                               (continue-ability state :corp (reorder-choice :corp (vec remaining)) card nil)
                               (system-msg state :corp (str "uses Bacterial Programming to add " (count to-hq)
                                                            " cards to HQ, discard " (count to-trash)
                                                            ", and arrange the top cards of R&D")))
                             (continue-ability state :corp (hq-step
                                                             (clojure.set/difference (set remaining) (set [target]))
                                                             to-trash
                                                             (conj to-hq target)) card nil)))})
           (trash-step [remaining to-trash]
             {:delayed-completion true
              :prompt "Select a card to discard"
              :choices (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (continue-ability state :corp (hq-step remaining to-trash `()) card nil)
                             (continue-ability state :corp (trash-step
                                                             (clojure.set/difference (set remaining) (set [target]))
                                                             (conj to-trash target)) card nil)))})]
     (let [arrange-rd (effect (continue-ability
                                {:optional
                                 {:delayed-completion true
                                  :prompt "Arrange top 7 cards of R&D?"
                                  :yes-ability {:delayed-completion true
                                                :effect (req (let [c (take 7 (:deck corp))]
                                                               (when (:run @state)
                                                                (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                                                               (show-wait-prompt state :runner "Corp to use Bacterial Programming")
                                                               (continue-ability state :corp (trash-step c `()) card nil)))}}}
                                card nil))]
       {:effect arrange-rd
        :delayed-completion true
        :stolen {:delayed-completion true
                 :effect arrange-rd}
        :interactive (req true)}))})