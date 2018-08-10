(ns game-test.cards.icebreakers.faust
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest faust
  (testing "Basic test: Pump by discarding"
    (do-game
      (new-game {:runner {:deck ["Faust" (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 4 (:current-strength (refresh faust))) "4 current strength")
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))
  (testing "Pump does not trigger trash prevention. #760"
    (do-game
      (new-game {:runner {:deck ["Faust"
                                 "Sacrificial Construct"
                                 "Fall Guy"
                                 "Astrolabe"
                                 "Gordian Blade"
                                 "Armitage Codebusting"]}})
      (take-credits state :corp)
      (core/draw state :runner 1)
      (play-from-hand state :runner "Faust")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Sacrificial Construct")
      (is (= 2 (count (get-resource state))) "Resources installed")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Astrolabe" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for hardware")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Gordian Blade" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for program")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Armitage Codebusting" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for resource")))))
