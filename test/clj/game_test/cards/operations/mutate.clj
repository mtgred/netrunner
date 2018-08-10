(ns game-test.cards.operations.mutate
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mutate
  ;; Mutate - trash a rezzed piece of ice, install and rez one from R&D
  (testing "Basic operation"
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma is installed")
      (is (:rezzed (get-ice state :hq 0)) "Enigma is rezzed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Installed card name was logged")))
  (testing "No ice in R&D"
    (do-game
      (new-game {:corp {:deck ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"]}})
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (= 1 (count (get-ice state :hq))) "1 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is installed")
      (play-from-hand state :corp "Mutate")
      (click-card state :corp (get-ice state :hq 0))
      (is (empty? (get-ice state :hq)) "No ice installed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged"))))
