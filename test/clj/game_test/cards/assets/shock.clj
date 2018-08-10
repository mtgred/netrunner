(ns game-test.cards.assets.shock
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shock
  ;; Shock! - do 1 net damage on access
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Shock!" 3)]}})
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (run-empty-server state "Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner took 1 net damage")))
  (testing "ensure :access flag is cleared on run end. Issue #2319"
    (do-game
      (new-game {:corp {:deck [(qty "Shock!" 3) "Chairman Hiro"]}})
      (trash-from-hand state :corp "Shock!")
      (play-from-hand state :corp "Shock!" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
      (is (not (:run @state)) "Run is complete")
      (trash-from-hand state :corp "Chairman Hiro")
      (is (= 2 (count (:discard (get-corp)))) "Hiro and Shock still in archives")
      (is (zero? (count (:scored (get-runner)))) "Hiro not scored by Runner"))))
