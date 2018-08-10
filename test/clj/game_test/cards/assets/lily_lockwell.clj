(ns game-test.cards.assets.lily-lockwell
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lily-lockwell
  ;; Lily Lockwell
  (do-game
    (new-game {:corp {:deck ["Lily Lockwell" "Beanstalk Royalties" (qty "Fire Wall" 10)]}})
    (core/gain state :corp :click 10)
    (starting-hand state :corp ["Lily Lockwell" "Beanstalk Royalties"])
    (play-from-hand state :corp "Lily Lockwell" "New remote")
    (core/gain state :runner :tag 2)
    (let [lily (get-content state :remote1 0)
          clicks (:click (get-corp))
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))
          hand (-> (get-corp) :hand count)]
      (core/rez state :corp lily)
      (is (= (+ 3 hand) (-> (get-corp) :hand count)) "Rezzing Lily Lockwell should draw 3 cards")
      (core/move state :corp (find-card "Beanstalk Royalties" (:hand (get-corp))) :deck)
      (card-ability state :corp (refresh lily) 0)
      (click-prompt state :corp (find-card "Beanstalk Royalties" (-> (get-corp) :prompt first :choices)))
      (is (= "Beanstalk Royalties" (-> (get-corp) :deck first :title)) "Beanstalk Royalties should be moved to top of R&D")
      (is (= 1 (:tag (get-runner))) "Runner should have 1 tag from Lily Lockwell ability")
      (is (= (- clicks 1) (:click (get-corp))) "Lily Lockwell ability should cost 1 click")
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck")
      (core/draw state :corp)
      (card-ability state :corp (refresh lily) 0)
      (click-prompt state :corp "Cancel")
      (is (last-log-contains? state "did not find") "Lily Lockwell's ability didn't find an operation")
      (is (zero? (:tag (get-runner))) "Runner should have 0 tags from Lily Lockwell ability even when no operation found"))))
