(ns game-test.cards.assets.chief-slee
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chief-slee
  ;; Chief Slee
  (do-game
    (new-game {:corp {:deck ["Chief Slee" "Hive" "Hedge Fund"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hive" "HQ")
    (play-from-hand state :corp "Chief Slee" "New remote")
    (run-on state :hq)
    (let [slee (get-content state :remote1 0)
          hive (get-ice state :hq 0)]
      (core/rez state :corp hive)
      (card-subroutine state :corp hive 0)
      (dotimes [_ 5]
        (card-ability state :corp slee 0))
      (take-credits state :runner)
      (card-ability state :corp slee 1)
      (is (= 5 (count (:discard (get-runner)))) "Chief Slee should do 5 meat damage"))))
