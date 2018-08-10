(ns game-test.cards.assets.bio-ethics-association
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bio-ethics-association
  ;; Bio-Ethics Association
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Bio-Ethics Association"]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))))))
  (testing "should be able to prevent damage from multiple copies"
    (do-game
      (new-game {:corp {:deck [(qty "Bio-Ethics Association" 2)]}
                 :runner {:deck ["Feedback Filter" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (take-credits state :runner)
      (let [filter (get-hardware state 0)]
        (is (= 1 (count (:prompt (get-runner)))) "Runner has a single damage prevention prompt")
        (card-ability state :runner filter 0)
        (click-prompt state :runner "Done")
        (is (zero? (count (:discard (get-runner)))) "Runner prevented damage")
        (is (= 1 (count (:prompt (get-runner)))) "Runner has a next damage prevention prompt")
        (click-prompt state :runner "Done")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")))))
