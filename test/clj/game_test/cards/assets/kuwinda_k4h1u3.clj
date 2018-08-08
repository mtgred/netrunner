(ns game-test.cards.assets.kuwinda-k4h1u3
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kuwinda-k4h1u3
  ;; Kuwinda K4H1U3
  (do-game
    (new-game {:corp {:deck ["Kuwinda K4H1U3"]}})
    (core/gain state :corp :credit 100)
    (core/gain state :runner :credit 100)
    (play-from-hand state :corp "Kuwinda K4H1U3" "New remote")
    (let [kuwinda (get-content state :remote1 0)]
      (core/rez state :corp kuwinda)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (zero? (-> (get-corp) :prompt first :base)) "Base Trace should start at 0")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (-> (get-runner) :discard count)) "Runner shouldn't take any damage")
      (is (= 1 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain 1 power counter")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (= 1 (-> (get-corp) :prompt first :base)) "Base Trace should now start at 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (zero? (-> (get-runner) :discard count)) "Runner shouldn't take any damage")
      (is (= 2 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain another power counter")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp (refresh kuwinda) 0)
      (is (= 2 (-> (get-corp) :prompt first :base)) "Base Trace should be up to 2")
      (click-prompt state :corp "1")
      (click-prompt state :runner "0")
      (is (= 1 (-> (get-runner) :brain-damage)) "Trace succeeded so runner should take 1 brain damage")
      (is (= 1 (-> (get-runner) :discard count)) "Trace succeeded so runner should discard card from damage")
      (is (= 1 (-> (get-corp) :discard count)) "Kuwinda should be in Archives")
      (is (= "Kuwinda K4H1U3" (-> (get-corp) :discard first :title)) "Kuwinda should be in Archives")
      (core/end-phase-12 state :corp nil))))
