(ns game-test.cards.operations.an-offer-you-can-t-refuse
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest an-offer-you-can-t-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game {:corp {:deck ["Celebrity Gift" "An Offer You Can't Refuse"]}})
    (play-from-hand state :corp "An Offer You Can't Refuse")
    (click-prompt state :corp "R&D")
    (core/move state :corp (find-card "Celebrity Gift" (:hand (get-corp))) :discard)
    (is (= 2 (count (:discard (get-corp)))))
    (click-prompt state :runner "No")
    (is (= 1 (:agenda-point (get-corp))) "An Offer the Runner refused")
    (is (= 1 (count (:scored (get-corp)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-corp))))
    (is (= 1 (count (:discard (get-corp)))))
    (is (find-card "Celebrity Gift" (:discard (get-corp))))))
