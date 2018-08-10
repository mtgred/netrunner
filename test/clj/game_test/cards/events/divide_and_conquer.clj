(ns game-test.cards.events.divide-and-conquer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest divide-and-conquer
  ;; Divide and Conquer
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer"]}})
      (starting-hand state :corp ["Hostile Takeover" "Ice Wall" "Ice Wall"])
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-from-hand state :runner "Divide and Conquer")
      (run-successful state)
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "No action")
      (is (= 4 (-> (get-runner) :register :last-run core/total-cards-accessed)) "Runner should access 2 cards in Archives, 1 in R&D, and 1 in HQ")))
  (testing "with The Turning Wheel counters"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer" "The Turning Wheel"]}})
      (starting-hand state :corp (concat ["Hostile Takeover"]
                                         (repeat 4 "Ice Wall")))
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (let [ttw (get-resource state 0)]
        (core/add-counter state :runner ttw :power 4)
        (play-from-hand state :runner "Divide and Conquer")
        (card-ability state :runner ttw 0)
        (card-ability state :runner ttw 0)
        (run-successful state)
        ;; HQ
        (dotimes [_ 3]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner (-> (prompt-map :runner) :choices first)))
        ;; R&D
        (dotimes [_ 3]
          (click-prompt state :runner "Card from deck")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "No prompts after all accesses are complete")
        (is (= 2 (-> (get-runner) :register :last-run :access-bonus)) "The Turning Wheel should provide 2 additional accesses")
        (is (= 8 (-> (get-runner) :register :last-run core/total-cards-accessed)) "Runner should access 2 cards in Archives, 1 + 2 in R&D, and 1 + 2 in HQ")))))
