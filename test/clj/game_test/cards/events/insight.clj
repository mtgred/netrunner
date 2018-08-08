(ns game-test.cards.events.insight
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest insight
  ;; Insight
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Elizabeth Mills"
                             "Jackson Howard" "Director Haas"]}
               :runner {:deck ["Insight"]}})
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (is (zero? (count (:hand (get-corp)))))
    (is (= 4 (count (:deck (get-corp)))))
    (play-from-hand state :runner "Insight")
    (is (= :waiting (-> (get-runner) :prompt first :prompt-type)) "Runner is waiting for Corp to reorder")
    (click-prompt state :corp (find-card "Director Haas" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Elizabeth Mills" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "Done")
    (is (not= :waiting (-> (get-runner) :prompt first :prompt-type)) "Waiting prompt done")
    (is (= "Caprice Nisei" (:title (nth (:deck (get-corp)) 0))))
    (is (= "Jackson Howard" (:title (nth (:deck (get-corp)) 1))))
    (is (= "Elizabeth Mills" (:title (nth (:deck (get-corp)) 2))))
    (is (= "Director Haas" (:title (nth (:deck (get-corp)) 3))))))
