(ns game-test.cards.events.indexing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest indexing
  ;; Indexing - Full test
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"]}
               :runner {:deck ["Indexing"]}})
    (dotimes [_ 5] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (is (zero? (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (play-from-hand state :runner "Indexing")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (click-prompt state :runner "Replacement effect")
    (click-prompt state :runner (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :runner "Start over")
    (click-prompt state :runner (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (click-prompt state :runner "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))
