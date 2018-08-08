(ns game-test.cards.resources.john-masanori
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest john-masanori
  ;; John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run
  (do-game
    (new-game {:corp {:deck ["Crisium Grid"]}
               :runner {:deck [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               "Fall Guy"]}})
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (core/rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (core/gain state :runner :click 2 :credit 2)
    (play-from-hand state :runner "John Masanori")
    (is (= 4 (count (:hand (get-runner)))))
    (run-empty-server state "HQ")
    (click-prompt state :runner "Pay 5 [Credits] to trash") ; trash crisium #2433
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-runner)))) "1 card drawn from first successful run")
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-runner)))) "No card drawn from second successful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-runner))) "1 tag taken from first unsuccessful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-runner))) "No tag taken from second unsuccessful run")))
