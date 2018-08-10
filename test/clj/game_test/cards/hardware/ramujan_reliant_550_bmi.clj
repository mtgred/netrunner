(ns game-test.cards.hardware.ramujan-reliant-550-bmi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ramujan-reliant-550-bmi
  ;; Prevent up to X net or brain damage.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Data Mine" "Snare!"]}
                 :runner {:deck [(qty "Ramujan-reliant 550 BMI" 4)
                                 (qty "Sure Gamble" 6)]}})
      (starting-hand state :runner
                     ["Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Sure Gamble"])
      (play-from-hand state :corp "Data Mine" "Server 1")
      (play-from-hand state :corp "Snare!" "Server 1")
      (let [sn (get-content state :remote1 0)
            dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)
              rr2 (get-hardware state 1)
              rr3 (get-hardware state 2)]
          (run-on state "Server 1")
          (core/rez state :corp dm)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "1")
          (is (last-log-contains? state "Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented")
          (run-successful state)
          (take-credits state :runner)
          (take-credits state :corp)
          (play-from-hand state :runner "Ramujan-reliant 550 BMI")
          (run-empty-server state "Server 1")
          (click-prompt state :corp "Yes")
          (card-ability state :runner rr2 0)
          (click-prompt state :runner "3")
          (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 1 (count (:hand (get-runner)))) "3 net damage prevented")))))
  (testing "Prevent up to X net or brain damage. Empty stack"
    (do-game
      (new-game {:corp {:deck ["Data Mine"]}
                 :runner {:deck ["Ramujan-reliant 550 BMI" "Sure Gamble"]}})
      (play-from-hand state :corp "Data Mine" "Server 1")
      (let [dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)]
          (run-on state "Server 1")
          (core/rez state :corp dm)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "Done")
          (is (zero? (count (:hand (get-runner)))) "Not enough cards in Stack for Ramujan to work"))))))
