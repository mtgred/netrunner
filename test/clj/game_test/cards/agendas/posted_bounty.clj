(ns game-test.cards.agendas.posted-bounty
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest posted-bounty
  ;; Posted Bounty
  (testing "Forfeiting takes 1 bad publicity"
    (do-game
      (new-game {:corp {:deck ["Posted Bounty"]}})
      (play-and-score state "Posted Bounty")
      (click-prompt state :corp "Yes")
      (is (zero? (:agenda-point (get-corp))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (:bad-publicity (get-corp))) "Forfeiting takes 1 bad publicity")
      (is (= 1 (:tag (get-runner))) "Runner receives 1 tag forfeiting Posted Bounty")))
  (testing "Choosing not to forfeit scores normally"
    (do-game
      (new-game {:corp {:deck ["Posted Bounty"]}})
      (play-and-score state "Posted Bounty")
      (click-prompt state :corp "No")
      (is (= 1 (:agenda-point (get-corp))))
      (is (zero? (:bad-publicity (get-corp))))
      (is (zero? (:tag (get-runner)))))))
