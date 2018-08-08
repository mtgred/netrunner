(ns game-test.cards.assets.isabel-mcguire
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest isabel-mcguire
  ;; Isabel McGuire
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Isabel McGuire"]}})
    (play-from-hand state :corp "Isabel McGuire" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (zero? (-> (get-corp) :hand count)))
    (let [isabel (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp isabel)
      (card-ability state :corp isabel 0)
      (click-card state :corp (refresh iw))
      (is (= 1 (-> (get-corp) :hand count))))))
