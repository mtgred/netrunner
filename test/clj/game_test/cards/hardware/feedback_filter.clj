(ns game-test.cards.hardware.feedback-filter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game {:corp {:deck ["Data Mine"
                             "Cerebral Overwriter"
                             "Mushin No Shin"]}
               :runner {:deck [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Cerebral Overwriter" (:hand (get-corp))))
    (play-from-hand state :corp "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (is (= 3 (get-counters (refresh co) :advancement)) "3 advancements on Overwriter")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Feedback Filter")
      (is (= 7 (:credit (get-runner))))
      (let [ff (get-hardware state 0)]
        (run-on state "Server 1")
        (core/rez state :corp dm)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner ff 0)
        (click-prompt state :runner "Done")
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-successful state)
        (click-prompt state :corp "Yes") ; pay 3 to fire Overwriter
        (card-ability state :runner ff 1)
        (click-prompt state :runner "Done")
        (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-hardware state)) "Feedback Filter trashed")))))
