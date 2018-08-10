(ns game-test.cards.assets.commercial-bankers-group
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest commercial-bankers-group
  ;; Commercial Bankers Group - Gain 3 credits at turn start if unprotected by ice
  (do-game
    (new-game {:corp {:deck ["Commercial Bankers Group" "Ice Wall"]}})
    (play-from-hand state :corp "Commercial Bankers Group" "New remote")
    (let [cbg (get-content state :remote1 0)]
      (core/rez state :corp cbg)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Bankers Group paid 3 credits")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (is (= 11 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 11 (:credit (get-corp))) "Bankers Group didn't pay credits"))))
