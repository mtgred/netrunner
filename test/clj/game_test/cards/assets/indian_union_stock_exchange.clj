(ns game-test.cards.assets.indian-union-stock-exchange
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest indian-union-stock-exchange
  ;; Indian Union Stock Exchange
  (do-game
    (new-game {:corp {:id "Argus Security: Protection Guaranteed"
                      :deck ["Indian Union Stock Exchange" "Beanstalk Royalties"
                             "Kill Switch" "Net Police"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Indian Union Stock Exchange" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= (+ 3 credits) (:credit (get-corp))) "Corp should only gain 3 credits"))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Kill Switch")
      (is (= credits (:credit (get-corp))) "Corp should neither gain nor lose any credits"))
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "Net Police" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (is (= credits (:credit (get-corp))) "Corp should neither gain nor lose any credits"))))
