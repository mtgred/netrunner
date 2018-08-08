(ns game-test.cards.assets.security-subcontract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest security-subcontract
  ;; Security Subcontract
  (do-game
    (new-game {:corp {:deck ["Security Subcontract" "Ice Wall"]}})
    (play-from-hand state :corp "Security Subcontract" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ss (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp ss)
      (core/rez state :corp iw)
      (card-ability state :corp ss 0)
      (let [credits (:credit (get-corp))
            clicks (:click (get-corp))]
        (click-card state :corp iw)
        (is (= (+ credits 4) (:credit (get-corp))) "Corp should gain 4 from Security Subcontract ability")
        (is (= "Ice Wall" (-> (get-corp) :discard first :title)) "Ice Wall should be in Archives from Security Subcontract ability")
        (is (= (- clicks 1) (:click (get-corp))) "Corp should lose 1 click from Security Subcontract ability")))))
