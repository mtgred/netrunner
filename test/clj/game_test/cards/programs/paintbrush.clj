(ns game-test.cards.programs.paintbrush
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest paintbrush
  ;; Paintbrush - Give rezzed ICE a chosen subtype until the end of the next run
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Paintbrush"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paintbrush")
    (is (= 2 (core/available-mu state)))
    (let [iwall (get-ice state :hq 0)
          pb (get-program state 0)]
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (is (= 3 (:click (get-runner))) "Ice Wall not rezzed, so no click charged")
      (click-prompt state :runner "Done") ; cancel out
      (core/rez state :corp iwall)
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (click-prompt state :runner "Code Gate")
      (is (= 2 (:click (get-runner))) "Click charged")
      (is (= true (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (= false (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))
