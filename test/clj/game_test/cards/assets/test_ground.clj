(ns game-test.cards.assets.test-ground
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest test-ground
  ;; Test Ground
  (do-game
    (new-game {:corp {:deck ["Test Ground" "Ice Wall" "News Team"]}})
    (core/gain state :corp :credit 100 :click 100)
    (play-from-hand state :corp "Test Ground" "New remote")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "News Team" "New remote")
    (let [ground (get-content state :remote1 0)
          iw (get-ice state :remote2 0)
          news (get-content state :remote3 0)]
      (core/rez state :corp ground)
      (core/rez state :corp iw)
      (core/rez state :corp news)
      (advance state ground 2)
      (is (:rezzed (refresh iw)) "Ice Wall should be rezzed")
      (is (:rezzed (refresh news)) "News Team should be rezzed")
      (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
      (card-ability state :corp ground 0)
      (click-card state :corp iw)
      (click-card state :corp news)
      (is (not (:rezzed (refresh iw))) "Ice Wall should be rezzed")
      (is (not (:rezzed (refresh news))) "News Team should be rezzed")
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in discard"))))
