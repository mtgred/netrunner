(ns game-test.cards.assets.space-camp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest space-camp
  (testing "when in Archives. #1929"
    (do-game
      (new-game {:corp {:deck ["Space Camp" "News Team" "Breaking News"]}})
      (trash-from-hand state :corp "Space Camp")
      (trash-from-hand state :corp "News Team")
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "News Team")
      (click-prompt state :runner "Take 2 tags")
      (click-prompt state :runner "Space Camp")
      (click-prompt state :corp "Yes")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "Agenda advanced once from Space Camp")
      (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
      (is (not (:run @state)) "Run completed"))))
