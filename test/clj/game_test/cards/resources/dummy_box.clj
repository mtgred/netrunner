(ns game-test.cards.resources.dummy-box
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dummy-box
  ;; Dummy Box - trash a card from hand to prevent corp trashing installed card
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (core/trash state :runner (get-program state 0))
      (is (not-empty (:prompt (get-runner))) "Dummy Box prompting to prevent program trash")
      (card-ability state :runner (get-resource state 0) 2)
      (click-card state :runner (find-card "Clot" (:hand (get-runner))))
      (click-prompt state :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Clot trashed")
      (is (empty? (:hand (get-runner))) "Card trashed from hand")
      (is (= 1 (count (get-program state))) "Cache still installed")
      (is (= 1 (count (get-resource state))) "Dummy Box still installed")))
  (testing "doesn't prevent program deletion during purge"
    (do-game
      (new-game {:runner {:deck [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Clot")
      (take-credits state :runner)
      (core/purge state :corp)
      (is (empty? (:prompt (get-runner))) "Dummy Box not prompting to prevent purge trash"))))
