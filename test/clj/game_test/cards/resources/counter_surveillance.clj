(ns game-test.cards.resources.counter-surveillance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest counter-surveillance
  ;; Counter-Surveillance
  (testing "Trash to run, on successful run access cards equal to Tags and pay that amount in credits"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck ["Counter Surveillance"]}})
      (take-credits state :corp)
      (core/gain state :runner :tag 2)
      (play-from-hand state :runner "Counter Surveillance")
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (click-prompt state :runner "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (click-prompt state :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (click-prompt state :runner "No action")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits"))))
  (testing "Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck ["Counter Surveillance" "Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Counter Surveillance" "Obelus"])
      (take-credits state :corp)
      (core/gain state :runner :tag 2)
      (core/gain state :runner :credit 2)
      (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
      (play-from-hand state :runner "Counter Surveillance")
      (play-from-hand state :runner "Obelus")
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits") ; Runner has enough credits to pay for CS
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (click-prompt state :runner "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (click-prompt state :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Runner did draw cards from Obelus after all accesses are done")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (zero? (:credit (get-runner))) "Runner has no credits")))))
