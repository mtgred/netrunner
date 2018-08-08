(ns game-test.cards.ice.tmi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tmi
  ;; TMI
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["TMI"]}})
      (play-from-hand state :corp "TMI" "HQ")
      (let [tmi (get-ice state :hq 0)]
        (core/rez state :corp tmi)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (:rezzed (refresh tmi))))))
  (testing "Losing trace derezzes TMI"
    (do-game
      (new-game {:corp {:deck ["TMI"]}
                 :runner {:id "Sunny Lebeau: Security Specialist"
                          :deck [(qty "Blackmail" 3)]}})
      (play-from-hand state :corp "TMI" "HQ")
      (let [tmi (get-ice state :hq 0)]
        (core/rez state :corp tmi)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (not (:rezzed (refresh tmi))))))))
