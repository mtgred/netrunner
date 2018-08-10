(in-ns 'game.cards.resources)

(def card-definition-levy-advanced-research-lab
  {"Levy Advanced Research Lab"
   (letfn [(lab-keep [cards]
             {:prompt "Choose a program to keep"
              :choices (cons "None" (filter #(= "Program" (:type %)) cards))
              :async true
              :msg (msg (if (= target "None") "take no card to their grip" (str "take " (-> target :title) " to their grip")))
              :effect (req (when (not= target "None")
                             (move state side target :hand))
                           (if (not-empty cards)
                             (let [tobottom (remove #(= % target) cards)]
                               (continue-ability state side
                                                 (reorder-choice :runner :corp tobottom '()
                                                                 (count tobottom) tobottom "bottom")
                                                 card nil))
                             (do (clear-wait-prompt state :corp)
                                 (effect-completed state side eid))))})]
     {:abilities [{:cost [:click 1]
                   :label "Reveal top 4 cards of stack"
                   :msg (msg "reveals the top 4 cards of the stack: "
                          (join ", " (map :title (take 4 (:deck runner)))))
                   :async true
                   :effect (req (show-wait-prompt state :corp "Runner to choose card to keep")
                                (let [from (take 4 (:deck runner))]
                                  (continue-ability state side (lab-keep from) card nil)))}]})})
