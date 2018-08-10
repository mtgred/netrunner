(ns game-test.cards.assets.snare
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest snare
  (testing "Basic test"
    ;; pay 4 on access, and do 3 net damage and give 1 tag
    (do-game
      (new-game {:corp {:deck [(qty "Snare!" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (= 3 (:credit (get-corp))) "Corp had 7 and paid 4 for Snare! 1 left")
      (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
      (is (zero? (count (:hand (get-runner)))) "Runner took 3 net damage")))
  (testing "Can't afford"
    (do-game
      (new-game {:corp {:deck ["Snare!"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (take-credits state :corp)
      (core/lose state :corp :credit 7)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (zero? (:tag (get-runner))) "Runner has 0 tags")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (zero? (count (:discard (get-runner)))) "Runner took no damage")))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck ["Snare!" "Dedicated Response Team"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-from-hand state :corp "Snare!" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/gain state :corp :click 1 :credit 4)
      (let [drt (get-content state :remote2 0)]
        (take-credits state :corp)
        (run-on state "Server 1")
        (core/rez state :corp drt)
        (run-successful state)
        (is (= :waiting (-> @state :runner :prompt first :prompt-type))
            "Runner has prompt to wait for Snare!")
        (click-prompt state :corp "Yes")
        (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        (is (= 5 (count (:discard (get-runner)))) "Runner took 5 damage")))))
