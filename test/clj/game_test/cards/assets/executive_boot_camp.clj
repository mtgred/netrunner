(ns game-test.cards.assets.executive-boot-camp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest executive-boot-camp
  ;; Executive Boot Camp
  (testing "suppress the start-of-turn event on a rezzed card. Issue #1346"
    (do-game
      (new-game {:corp {:deck ["Eve Campaign" "Executive Boot Camp"]}})
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Executive Boot Camp" "New remote")
      (take-credits state :corp)
      (is (= 6 (:credit (get-corp))) "Corp ends turn with 6 credits")
      (let [eve (get-content state :remote1 0)
            ebc (get-content state :remote2 0)]
        (core/rez state :corp ebc)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (click-card state :corp eve)
        (is (= 2 (:credit (get-corp))) "EBC saved 1 credit on the rez of Eve")
        (is (= 16 (get-counters (refresh eve) :credit)))
        (core/end-phase-12 state :corp nil)
        (is (= 2 (:credit (get-corp))) "Corp did not gain credits from Eve")
        (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
        (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve"))))
  (testing "works with Ice that has alternate rez costs"
    (do-game
      (new-game {:corp {:deck ["15 Minutes" "Executive Boot Camp"
                               "Tithonium"]}})
      (core/gain state :corp :credit 3)
      (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
      (play-from-hand state :corp "Tithonium" "HQ")
      (play-from-hand state :corp "Executive Boot Camp" "New remote")
      (let [ebc (get-content state :remote1 0)
            tith (get-ice state :hq 0)]
        (core/rez state :corp ebc)
        (take-credits state :corp)
        (is (= 9 (:credit (get-corp))) "Corp ends turn with 9 credits")
        (take-credits state :runner)
        (is (not (:rezzed (refresh tith))) "Tithonium not rezzed")
        (is (:corp-phase-12 @state) "Corp in Step 1.2")
        (card-ability state :corp ebc 0)
        (click-card state :corp tith)
        (click-prompt state :corp "No")
        (is (and (:installed (refresh tith)) (:rezzed (refresh tith))) "Rezzed Tithonium")
        (is (= 1 (:credit (get-corp))) "EBC saved 1 credit on the rez of Tithonium")))))
