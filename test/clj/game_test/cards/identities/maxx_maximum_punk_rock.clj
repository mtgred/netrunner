(ns game-test.cards.identities.maxx-maximum-punk-rock
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest maxx-maximum-punk-rock
  ;; MaxX
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 "Eater"]}})
      (starting-hand state :runner ["Eater"])
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "Wyldside, Wyldside")
          "Maxx did log trashed card names")))
  (testing "with Dummy Box. Check that mills don't trigger trash prevention #3246"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Dummy Box" 30)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (play-from-hand state :runner "Dummy Box")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-runner))) "Dummy Box not fired from mill")))
  (testing "with Wyldside - using Wyldside during Step 1.2 should lose 1 click"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 (qty "Sure Gamble" 3)
                                 (qty "Infiltration" 3)
                                 (qty "Corroder" 3)
                                 (qty "Eater" 3)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (starting-hand state :runner ["Wyldside"])
      (play-from-hand state :runner "Wyldside")
      (take-credits state :runner 3)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
      (is (find-card "Wyldside" (get-resource state)) "Wyldside was installed")
      (take-credits state :corp)
      (is (zero? (:click (get-runner))) "Runner has 0 clicks")
      (is (:runner-phase-12 @state) "Runner is in Step 1.2")
      (let [maxx (get-in @state [:runner :identity])
            wyld (find-card "Wyldside" (get-resource state))]
        (card-ability state :runner maxx 0)
        (card-ability state :runner wyld 0)
        (core/end-phase-12 state :runner nil)
        (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
        (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
        (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total")))))
