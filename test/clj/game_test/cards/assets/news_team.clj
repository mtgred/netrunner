(ns game-test.cards.assets.news-team
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (do-game
    (new-game {:corp {:deck [(qty "News Team" 3) "Blacklist"]}})
    (trash-from-hand state :corp "News Team")
    (play-from-hand state :corp "Blacklist" "New remote")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :runner "Take 2 tags")
    (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
    (run-empty-server state :archives)
    (click-prompt state :runner "Add News Team to score area")
    (is (= 1 (count (:scored (get-runner)))) "News Team added to Runner score area")
    (trash-from-hand state :corp "News Team")
    (core/rez state :corp (get-content state :remote1 0))
    (run-empty-server state :archives)
    (click-prompt state :runner "Add News Team to score area")
    (is (= 2 (count (:scored (get-runner)))) "News Team added to Runner score area with Blacklist rez")))
