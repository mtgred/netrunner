(ns game-test.cards.ice.gemini
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gemini
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Gemini" (qty "Hedge Fund" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]}})
      (play-from-hand state :corp "Gemini" "HQ")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (let [gem (get-ice state :hq 0)]
        (run-on state "HQ")
        (core/rez state :corp gem)
        (card-subroutine state :corp gem 0)
        (click-prompt state :corp "3") ; boost to trace strength 5
        (click-prompt state :runner "0")
        (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
        (card-subroutine state :corp gem 0)
        (click-prompt state :corp "3") ; boost to trace strength 5
        (click-prompt state :runner "5") ; match trace
        (is (= 3 (count (:discard (get-runner)))) "Did only 1 net damage for having trace strength 5 or more"))))
  (testing "Interaction with Chronos Protocol and kicker"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Gemini" (qty "Hedge Fund" 2)]}
                 :runner {:deck ["Sure Gamble" (qty "Dirty Laundry" 2)]}})
      (play-from-hand state :corp "Gemini" "HQ")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (let [gem (get-ice state :hq 0)]
        (run-on state "HQ")
        (core/rez state :corp gem)
        (card-subroutine state :corp gem 0)
        (click-prompt state :corp "3") ; boost to trace strength 5
        (click-prompt state :runner "0")
        (click-prompt state :corp "Yes")
        (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")))))
