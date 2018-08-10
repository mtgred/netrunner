(ns game-test.cards.operations.precognition
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game {:corp {:deck ["Precognition" "Caprice Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"]}})
    (starting-hand state :corp ["Precognition"])
    (play-from-hand state :corp "Precognition")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :corp "Start over")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (click-prompt state :corp "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))
