(ns game-test.cards.assets.mr-stone
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mr-stone
  ;; Mr Stone
  (do-game
    (new-game {:corp {:deck ["Mr. Stone"]}})
    (play-from-hand state :corp "Mr. Stone" "New remote")
    (let [stone (get-content state :remote1 0)]
      (core/rez state :corp stone)
      (core/gain-tags state :runner 1)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 meat damage from gaining 1 tag")
      (core/gain-tags state :corp 5)
      (is (= 2 (-> (get-runner) :discard count)) "Runner should take 1 meat damage from gaining 5 tags"))))
