(ns game-test.cards.hardware.autoscripter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest autoscripter
  ;; Autoscripter - gain 1 [Click] first time Runner installs program from Grip during their turn.
  ;; Trash if unsuccessful run
  (do-game
    (new-game {:runner {:deck ["Autoscripter" (qty "Inti" 3) "Clone Chip"]}
               :options {:start-as :runner}})
    (testing "Gaining (and not gaining) clicks"
      (play-from-hand state :runner "Inti")
      (play-from-hand state :runner "Autoscripter")
      (play-from-hand state :runner "Inti")
      (is (= 1 (:click (get-runner))) "Did not gain a click when installing program from hand a second time")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Inti")
      (is (= 4 (:click (get-runner))) "Gained 1 click when installing Program from hand")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (core/trash state :runner (get-program state 0))
      (card-ability state :runner (get-hardware state 1) 0)
      (click-card state :runner (first (:discard (get-runner))))
      (is (= 3 (count (get-program state))) "Three Intis installed")
      (is (= 3 (:click (get-runner))) "Did not gain a click from installing a Program from heap"))
    (testing "Trashing on unsuccessful run"
      (run-on state :hq)
      (run-jack-out state)
      (is (= "Autoscripter" (:title (last (:discard (get-runner))))) "Autoscripter was trashed after successful run"))))
