(ns game-test.cards.upgrades.georgia-emelyov
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest georgia-emelyov
  ;; Georgia Emelyov
  (do-game
    (new-game {:corp {:deck ["Georgia Emelyov"]}})
    (play-from-hand state :corp "Georgia Emelyov" "New remote")
    (let [geo (get-content state :remote1 0)]
      (core/rez state :corp geo)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-jack-out state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (card-ability state :corp (refresh geo) 0)
      (click-prompt state :corp "Archives")
      (let [geo (get-content state :archives 0)]
        (is geo "Georgia moved to Archives")
        (run-on state "Archives")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-runner)))) "Runner took 1 net damage")
        (run-on state "HQ")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-runner)))) "Runner did not take damage")))))
