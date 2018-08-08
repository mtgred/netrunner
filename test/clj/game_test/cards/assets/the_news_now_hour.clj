(ns game-test.cards.assets.the-news-now-hour
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-news-now-hour
  ;; The News Now Hour
  (do-game
    (new-game {:corp {:deck ["The News Now Hour"]}
               :runner {:deck ["Rumor Mill"]}})
    (play-from-hand state :corp "The News Now Hour" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Rumor Mill")
    (is (= 1 (-> (get-runner) :hand count)) "Rumor Mill should still be in hand after trying to play it")))
