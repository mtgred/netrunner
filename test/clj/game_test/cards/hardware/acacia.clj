(ns game-test.cards.hardware.acacia
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest acacia
  ;; Acacia - Optionally gain credits for number of virus tokens then trash
  (do-game
    (new-game {:runner {:deck ["Acacia" "Virus Breeding Ground" "Datasucker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Acacia")
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Datasucker")
    (core/add-counter state :runner (get-resource state 0) :virus 4)
    (core/add-counter state :runner (get-program state 0) :virus 3)
    (take-credits state :runner)
    (is (= 2 (:credit (get-runner))) "Runner initial credits")
    (core/purge state :corp)
    (click-prompt state :runner "Yes")
    (is (= 9 (:credit (get-runner))) "Runner gained 9 credits")
    (is (= 1 (count (:discard (get-runner)))) "Acacia has trashed")))
