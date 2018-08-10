(ns game-test.cards.ice.kakugo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kakugo
  ;; Kakugo
  (testing "ability continues to work when ice is swapped"
    (do-game
      (new-game {:corp {:deck ["Kakugo"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Kakugo" "R&D")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (let [kakugo   (get-ice state :rd 0)
            ice-wall (get-ice state :archives 0)]
        (run-on state "R&D")
        (core/rez state :corp kakugo)
        (run-continue state)
        (run-jack-out state)
        (is (= 2 (count (:hand (get-runner)))) "Runner took damage before swap")
        (core/swap-ice state :corp (refresh kakugo) (refresh ice-wall))
        (run-on state "Archives")
        (run-continue state)
        (run-jack-out state)
        (is (= 1 (count (:hand (get-runner)))) "Runner took damage after swap")))))
