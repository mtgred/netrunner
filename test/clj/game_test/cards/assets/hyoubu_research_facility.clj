(ns game-test.cards.assets.hyoubu-research-facility
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hyoubu-research-facility
  ;; Hyoubu Research Facility
  (do-game
    (new-game {:corp {:deck ["Hyoubu Research Facility" "Snowflake"]}})
    (play-from-hand state :corp "Hyoubu Research Facility" "New remote")
    (play-from-hand state :corp "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp hrf)
      (core/rez state :corp sf)
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 3 (:credit (get-corp))) "No credits gained from Hyoubu"))))
