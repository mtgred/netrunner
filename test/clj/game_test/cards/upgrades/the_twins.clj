(ns game-test.cards.upgrades.the-twins
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-twins
  ;; The Twins
  (do-game
    (new-game {:corp {:deck ["The Twins" (qty "Ice Wall" 10)]}
               :runner {:deck ["Corroder"]}})
    (starting-hand state :corp ["The Twins" "Ice Wall" "Ice Wall"])
    (play-from-hand state :corp "The Twins" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (let [twins (get-content state :remote1 0)
          iw (get-ice state :remote1 0)]
      (core/rez state :corp twins)
      (core/rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [cor (get-program state 0)]
        (run-on state "Server 1")
        (card-ability state :runner cor 0)
        (run-continue state)
        (is (zero? (-> @state :run :position)) "Run should be at position 0")
        (card-ability state :corp twins 0)
        (click-card state :corp (-> (get-corp) :hand first))
        (is (= 1 (-> @state :run :position)) "Run should be moved back to position 1")))))
