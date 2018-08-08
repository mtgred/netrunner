(ns game-test.cards.events.emergent-creativity
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest emergent-creativity
  ;; Emergent Creativty - Double, discard programs/hardware from grip, install from heap
  (do-game
    (new-game {:runner {:deck ["Emergent Creativity" "Paperclip"
                               "Heartbeat" "Gordian Blade" "Test Run"]}})
    (starting-hand state :runner ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Test Run"])
    (take-credits state :corp)
    (play-from-hand state :runner "Emergent Creativity")
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (click-card state :runner (find-card "Gordian Blade" (:hand (get-runner))))
    (click-prompt state :runner "Done")
    (click-prompt state :runner (find-card "Paperclip" (:deck (get-runner))))
    (is (= 3 (:credit (get-runner))) "Offset cost of installing Paperclip")
    (is (zero? (count (:deck (get-runner)))) "Installed from heap")
    (is (= 3 (count (:discard (get-runner)))) "Discard is 3 cards - EC, Heartbeat, GB")
    (is (= 2 (:click (get-runner))) "Emergent Creativity is a Double event")))
