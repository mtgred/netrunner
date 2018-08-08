(ns game-test.cards.operations.lateral-growth
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lateral-growth
  (do-game
    (new-game {:corp {:deck ["Lateral Growth" "Breaking News"]}})
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "Lateral Growth")
    (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= "Breaking News" (:title (get-content state :remote1 0)))
        "Breaking News installed by Lateral Growth")
    (is (= 7 (:credit (get-corp))))))
