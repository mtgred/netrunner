(ns game-test.cards.events.exploratory-romp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest exploratory-romp
  ;; Exploratory Romp - Remove advancements from card instead of accessing
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-successful state)
        (click-prompt state :runner "Replacement effect")
        (click-prompt state :runner "2")
        (click-card state :runner (refresh tg))
        (is (zero? (:tag (get-runner))) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed"))))
  (testing "Don't remove more than the existing number of advancement tokens"
    (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-successful state)
        (click-prompt state :runner "Replacement effect")
        (click-prompt state :runner "3")
        (click-card state :runner (refresh tg))
        (is (zero? (:tag (get-runner))) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed")))))
