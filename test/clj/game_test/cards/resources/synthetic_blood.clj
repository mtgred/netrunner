(ns game-test.cards.resources.synthetic-blood
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Data Mine" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Synthetic Blood" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]}})
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (run-on state "HQ")
        (core/rez state :corp first-dm)
        (card-subroutine state :corp first-dm 0)
        (is (= 4 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :corp second-dm)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "no card drawn when receiving damage (2nd time)"))))
  (testing "The first and second time you take damage each turn (with GCS installed), draw one card"
    (do-game
      (new-game {:corp {:deck [(qty "Data Mine" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Synthetic Blood" 3)
                                 "Sure Gamble"
                                 (qty "Gene Conditioning Shoppe" 3)]}})
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (run-on state "HQ")
        (core/rez state :corp first-dm)
        (card-subroutine state :corp first-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :corp second-dm)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (2nd time)")))))
