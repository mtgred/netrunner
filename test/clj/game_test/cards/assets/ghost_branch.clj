(ns game-test.cards.assets.ghost-branch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Runner tags equal to advancements when accessed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ghost Branch"]}})
      (play-from-hand state :corp "Ghost Branch" "New remote")
      (let [gb (get-content state :remote1 0)]
        (core/advance state :corp {:card (refresh gb)})
        (core/advance state :corp {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)))
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (click-prompt state :corp "Yes") ; choose to do the optional ability
        (is (= 2 (:tag (get-runner))) "Runner given 2 tags"))))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck ["Ghost Branch" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Ghost Branch" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/gain state :corp :click 1)
      (let [gb (get-content state :remote1 0)
            drt (get-content state :remote2 0)]
        (core/advance state :corp {:card gb})
        (core/advance state :corp {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)) "Ghost Branch advanced twice")
        (take-credits state :corp)
        (run-on state "Server 1")
        (core/rez state :corp drt)
        (run-successful state)
        (is (prompt-is-type? state :runner :waiting) "Runner has prompt to wait for Ghost Branch")
        (click-prompt state :corp "Yes")
        (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
        (click-prompt state :runner "Pay 0 [Credits] to trash")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage")))))
