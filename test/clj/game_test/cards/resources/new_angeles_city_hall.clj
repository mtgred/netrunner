(ns game-test.cards.resources.new-angeles-city-hall
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; trash when agenda is stolen
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["SEA Source" "Breaking News"]}
                 :runner {:deck ["New Angeles City Hall"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "New Angeles City Hall")
      (let [nach (get-resource state 0)]
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0") ; default trace
        (click-prompt state :runner "0") ; Runner won't match
        (card-ability state :runner nach 0)
        (click-prompt state :runner "Done")
        (is (zero? (:tag (get-runner))) "Avoided SEA Source tag")
        (is (= 4 (:credit (get-runner))) "Paid 2 credits")
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (is (= 1 (:agenda-point (get-runner))))
        (is (empty? (get-resource state)) "NACH trashed by agenda steal"))))
  (testing "don't gain Siphon credits until opportunity to avoid tags has passed"
    (do-game
      (new-game {:runner {:deck ["Account Siphon" "New Angeles City Hall"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "New Angeles City Hall")
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "Replacement effect")
      (let [nach (get-resource state 0)]
        (is (= 4 (:credit (get-runner))) "Have not gained Account Siphon credits until tag avoidance window closes")
        (card-ability state :runner nach 0)
        (card-ability state :runner nach 0)
        (click-prompt state :runner "Done")
        (is (zero? (:tag (get-runner))) "Tags avoided")
        (is (= 10 (:credit (get-runner))) "10 credits siphoned")
        (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))))
