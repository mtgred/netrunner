(ns game-test.cards.resources.activist-support
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest activist-support
  ;; Activist Support - Take tag if you have none; Corp gains bad pub if they have none
  (do-game
    (new-game {:runner {:deck ["Activist Support"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Activist Support")
    (is (zero? (:tag (get-runner))))
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag; had none")
    (is (zero? (:bad-publicity (get-corp))))
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp took 1 bad pub; had none")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Runner had 1 tag; didn't take another")
    (take-credits state :corp)
    (is (= 1 (:bad-publicity (get-corp))) "Corp had 1 bad pub; didn't take another")))
