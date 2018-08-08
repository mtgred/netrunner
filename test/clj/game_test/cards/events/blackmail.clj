(ns game-test.cards.events.blackmail
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest blackmail
  ;; Prevent rezzing of ice for one run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Blackmail" 3)]}})
      (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Blackmail")
      (click-prompt state :runner "HQ")
      (let [iwall1 (get-ice state :hq 0)
            iwall2 (get-ice state :hq 1)]
        (core/rez state :corp iwall1)
        (is (not (:rezzed (refresh iwall1))) "First Ice Wall is not rezzed")
        (run-continue state)
        (core/rez state :corp iwall2)
        (is (not (:rezzed (refresh iwall2))) "Second Ice Wall is not rezzed")
        (core/jack-out state :runner nil)
        ;; Do another run, where the ice should rez
        (run-on state "HQ")
        (core/rez state :corp iwall1)
        (is (:rezzed (refresh iwall1)) "First Ice Wall is rezzed"))))
  (testing "Regression test for a rezzed tmi breaking game state on a blackmail run"
    (do-game
      (new-game {:corp {:deck [(qty "TMI" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Blackmail" 3)]}})
      (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
      (play-from-hand state :corp "TMI" "HQ")
      (let [tmi (get-ice state :hq 0)]
        (core/rez state :corp tmi)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (:rezzed (refresh tmi)) "TMI is rezzed")
        (take-credits state :corp)
        (play-from-hand state :runner "Blackmail")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (run-jack-out state)
        (run-on state "Archives")))))
