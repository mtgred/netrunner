(ns game-test.cards.resources.no-one-home
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest no-one-home
  ;; Prevent first tag or net damage of the turn if you beat trace0, then trash
  (do-game
    (new-game {:corp {:deck ["Data Mine" "SEA Source" "Scorched Earth"]}
               :runner {:deck [(qty "No One Home" 3) (qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Data Mine" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "No One Home")
    (let [dm (get-ice state :remote1 0)
          noh (get-resource state 0)]
      (run-on state "Server 1")
      (core/rez state :corp dm)
      (card-subroutine state :corp dm 0)
      (card-ability state :runner noh 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Done")
      (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
      (run-successful state)
      (play-from-hand state :runner "No One Home")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count (:prompt (get-runner)))) "Runner prompted to avoid tag")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Done")
      (is (= 3 (count (:discard (get-runner)))) "Two NOH trashed, 1 gamble played")
      (is (zero? (:tag (get-runner))) "Tags avoided")
      (take-credits state :corp)
      (play-from-hand state :runner "No One Home")
      (take-credits state :runner)
      (core/gain state :runner :tag 1)
      (core/gain state :corp :credit 4)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:prompt (get-runner)))) "Runner not prompted to avoid meat damage"))))
