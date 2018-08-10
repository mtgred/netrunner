(in-ns 'game.cards.hardware)

(def card-definition-top-hat
  {"Top Hat"
   (letfn [(ability [n]
             {:async true
              :mandatory true
              :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
              :choices (take n ["1" "2" "3" "4" "5"])
              :effect (effect (system-msg (str "accesses the card at position " (str->int target) " of R&D"))
                              (access-card eid (nth (:deck corp) (dec (str->int target))) "an unseen card"))})]
     {:events {:successful-run
               {:req (req (= target :rd))
                :interactive (req true)
                :optional {:prompt "Use Top Hat to choose one of the top 5 cards in R&D to access?"
                           :yes-ability {:effect (req (swap! state assoc-in [:run :run-effect :replace-access]
                                                             (ability (count (:deck corp)))))}}}}})})
