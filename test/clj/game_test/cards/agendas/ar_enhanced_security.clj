(ns game-test.cards.agendas.ar-enhanced-security
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ar-enhanced-security
  ;; AR-Enhanced Security
  (do-game
    (new-game {:corp {:deck ["AR-Enhanced Security" (qty "NGO Front" 3)]}})
    (testing "set up"
      (core/gain state :corp :click 10 :credit 10)
      (core/gain state :runner :credit 10)
      (dotimes [_ 3]
        (play-from-hand state :corp "NGO Front" "New remote"))
      (take-credits state :corp))
    (testing "don't take a tag from trashing normally"
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 1 (count (:discard (get-corp)))) "trashed")
      (is (zero? (:tag (get-runner))) "Runner took 0 tags")
      (take-credits state :runner)
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :corp))
    (testing "gain a tag from first trash"
      (run-on state :remote2)
      (run-successful state)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 2 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag"))
    (testing "don't gain a tag from second trash"
      (run-on state :remote3)
      (run-successful state)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 3 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (:tag (get-runner))) "Runner took 0 tags"))))
