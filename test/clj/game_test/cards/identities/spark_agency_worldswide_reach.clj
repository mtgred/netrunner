(ns game-test.cards.identities.spark-agency-worldswide-reach
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest spark-agency-worldswide-reach
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game {:corp {:id "Spark Agency: Worldswide Reach"
                      :deck [(qty "Launch Campaign" 3)]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :corp lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (core/rez state :corp lc3)
      (is (= 4 (:credit (get-runner)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/rez state :corp lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))
