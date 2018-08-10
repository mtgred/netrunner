(ns game-test.cards.programs.kyuban
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kyuban
  ;; Kyuban
  (testing "Gain creds when passing a piece of ice, both when rezzed and when unrezzed."
    (do-game
      (new-game {:corp {:deck [(qty "Lockdown" 3)]}
                 :runner {:deck [(qty "Kyuban" 1)]}})
      (play-from-hand state :corp "Lockdown" "HQ")
      (play-from-hand state :corp "Lockdown" "Archives")
      (let [ld1 (get-ice state :archives 0)
            ld2 (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner ld1)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= starting-creds (:credit (get-runner))) "Gained no money for passing other ice")
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Gained 2 creds for passing unrezzed host ice"))
        (let [starting-creds-2 (:credit (get-runner))]
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/rez state :corp ld1)
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds-2 2) (:credit (get-runner)))
              "Gained 2 creds for passing rezzed host ice"))))))
