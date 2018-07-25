(in-ns 'game.cards.agendas)

(def card-definition-reeducation
  {"Reeducation"
   (letfn [(corp-final [chosen original]
             {:prompt (str "The bottom cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :msg (req (let [n (count chosen)]
                          (str "add " n " cards from HQ to the bottom of R&D and draw " n " cards.
                          The Runner randomly adds " (if (<= n (count (:hand runner))) n 0) " cards from their Grip
                          to the bottom of the Stack")))
              :effect (req (let [n (count chosen)]
                             (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck))
                                 (draw state :corp n)
                                 ; if corp chooses more cards than runner's hand, don't shuffle runner hand back into Stack
                                 (when (<= n (count (:hand runner)))
                                   (doseq [r (take n (shuffle (:hand runner)))] (move state :runner r :deck)))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid))
                             (continue-ability state side (corp-choice original '() original) card nil))))})
           (corp-choice [remaining chosen original] ; Corp chooses cards until they press 'Done'
             {:prompt "Choose a card to move to bottom of R&D"
              :choices (conj (vec remaining) "Done")
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (not= target "Done")
                               (continue-ability
                                 state side
                                 (corp-choice (remove-once #(= target %) remaining) chosen original)
                                 card nil)
                               (if (pos? (count (remove #(= % "Done") chosen)))
                                 (continue-ability state side (corp-final (remove #(= % "Done") chosen) original) card nil)
                                 (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                                     (clear-wait-prompt state :runner)
                                     (effect-completed state side eid))))))})]
   {:async true
    :effect (req (show-wait-prompt state :runner "Corp to add cards from HQ to bottom of R&D")
                 (let [from (get-in @state [:corp :hand])]
                   (if (pos? (count from))
                     (continue-ability state :corp (corp-choice from '() from) card nil)
                     (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                         (effect-completed state side eid)))))})})
