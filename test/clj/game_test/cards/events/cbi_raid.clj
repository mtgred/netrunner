(ns game-test.cards.events.cbi-raid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"]}
               :runner {:deck ["CBI Raid"]}})
    (take-credits state :corp)
    (is (= 5 (count (:hand (get-corp)))))
    (play-from-hand state :runner "CBI Raid")
    (is (= :hq (get-in @state [:run :server 0])))
    (run-successful state)
    (click-prompt state :corp (find-card "Caprice Nisei" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
    ;; try starting over
    (click-prompt state :corp "Start over")
    (click-prompt state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:hand (get-corp)))) ;this is the top card of R&D
    (click-prompt state :corp "Done")
    (is (zero? (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))
