(ns game-test.cards.ice.data-mine
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest data-mine
  ;; Data Mine - do one net and trash
  (do-game
    (new-game {:corp {:deck ["Data Mine"]}})
    (play-from-hand state :corp "Data Mine" "Server 1")
    (take-credits state :corp)
    (let [dm (get-ice state :remote1 0)]
      (run-on state "Server 1")
      (core/rez state :corp dm)
      (card-subroutine state :corp dm 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner suffered 1 net damage"))))
