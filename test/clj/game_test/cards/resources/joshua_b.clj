(ns game-test.cards.resources.joshua-b
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest joshua-b
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game {:runner {:deck ["Joshua B."]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Joshua B.")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (zero? (:click (get-runner))) "Runner has 0 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (:click (get-runner))) "Gained extra click from Joshua")
    (core/end-phase-12 state :runner nil)
    (is (= 5 (:click (get-runner))) "Gained normal clicks as well")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")))
