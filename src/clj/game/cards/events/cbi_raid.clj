(in-ns 'game.cards.events)

(def card-definition-cbi-raid
  {"CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid))
                             (continue-ability state side (cbi-choice original '() (count original) original)
                                               card nil)))})
           (cbi-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (cbi-choice (remove-once #(= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (cbi-final chosen original) card nil))))})]
     {:req (req hq-runnable)
            :async true
            :effect (effect (run :hq {:replace-access
                                {:msg "force the Corp to add all cards in HQ to the top of R&D"
                                 :async true
                                 :mandatory true
                                 :effect (req (show-wait-prompt state :runner "Corp to add all cards in HQ to the top of R&D")
                                              (let [from (:hand corp)]
                                                (if (pos? (count from))
                                                  (continue-ability state :corp (cbi-choice from '() (count from) from) card nil)
                                                  (do (clear-wait-prompt state :runner)
                                                      (effect-completed state side eid)))))}}
                                 card))})})
