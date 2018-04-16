(in-ns 'game.core)

(def card-operations-hasty-relocation
  {"Hasty Relocation"
   (letfn [(hr-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :delayed-completion true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid card))
                             (continue-ability state side (hr-choice original '() 3 original)
                                               card nil)))})
           (hr-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (hr-choice (remove-once #(= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (hr-final chosen original) card nil))))})]
     {:additional-cost [:mill 1]
      :delayed-completion true
      :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
      :effect (req (when-completed (draw state side 3 nil)
                                   (do (show-wait-prompt state :runner "Corp to add 3 cards in HQ to the top of R&D")
                                       (let [from (get-in @state [:corp :hand])]
                                         (continue-ability state :corp (hr-choice from '() 3 from) card nil)))))})})