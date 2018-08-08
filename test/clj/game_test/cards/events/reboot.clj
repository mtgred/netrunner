(ns game-test.cards.events.reboot
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reboot
  ;; Reboot - run on Archives, install 5 cards from head facedown
  (do-game
    (new-game {:runner {:deck ["Reboot" "Sure Gamble" "Paperclip" "Clot"]}})
    (take-credits state :corp)
    (trash-from-hand state :runner "Sure Gamble")
    (trash-from-hand state :runner "Paperclip")
    (trash-from-hand state :runner "Clot")
    (is (empty? (core/all-installed state :runner)) "Runner starts with no installed cards")
    (is (= 3 (count (:discard (get-runner)))) "Runner starts with 3 cards in trash")
    (is (empty? (:rfg (get-runner))) "Runner starts with no discarded cards")
    (play-from-hand state :runner "Reboot")
    (run-successful state)
    (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
    (click-card state :runner (find-card "Paperclip" (:discard (get-runner))))
    (click-card state :runner (find-card "Clot" (:discard (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 3 (count (filter :facedown (core/all-installed state :runner)))) "Runner has 3 facedown cards")
    (is (= 3 (count (core/all-installed state :runner))) "Runner has no other cards installed")
    (is (empty? (:discard (get-runner))) "Runner has empty trash")
    (is (= 1 (count (:rfg (get-runner)))) "Runner has 1 card in RFG")))
