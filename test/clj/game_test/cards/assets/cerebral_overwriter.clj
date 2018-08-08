(ns game-test.cards.assets.cerebral-overwriter
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cerebral-overwriter
  ;; Cerebral Overwriter
  (do-game
    (new-game {:corp {:deck ["Cerebral Overwriter"]}})
    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (let [co (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh co)})
      (core/advance state :corp {:card (refresh co)})
      (is (= 2 (get-counters (refresh co) :advancement)))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes") ; choose to do the optional ability
      (is (= 2 (:brain-damage (get-runner))) "Runner takes 2 brain damage"))))
