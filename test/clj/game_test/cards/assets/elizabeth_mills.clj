(ns game-test.cards.assets.elizabeth-mills
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-trash to trash a location
  (do-game
    (new-game {:corp {:deck ["Elizabeth Mills"]}
               :runner {:deck ["Earthrise Hotel"]}})
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (take-credits state :runner)
    (let [liz (get-content state :remote1 0)
          hotel (get-resource state 0)]
      (core/rez state :corp liz)
      (is (zero? (:bad-publicity (get-corp))) "1 bad publicity removed")
      (card-ability state :corp liz 0)
      (click-card state :corp hotel)
      (is (= 1 (count (:discard (get-runner)))) "Earthrise trashed")
      (is (= 1 (count (:discard (get-corp)))) "Elizabeth Mills trashed")
      (is (= 1 (:bad-publicity (get-corp))) "1 bad publicity taken from trashing a location"))))
