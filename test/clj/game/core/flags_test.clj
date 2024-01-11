(ns game.core.flags-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.flags :as flags]
   [game.macros :refer [req]]
   [game.test-framework :refer :all]))

(deftest can-score?-test
  (defmethod core/defcard-impl "Test Card" [_] {})
  (before-each [state (new-game)
                test-card {:title "Test Card" :type "Agenda" :advancementcost 2}
                test-card-with-counters (assoc test-card :advance-counter 2)]
    (testing "must be an agenda"
      (is (not (flags/can-score? state :corp {}))))
    (testing "advancement requirement skipped with no-req"
      (is (flags/can-score? state :corp test-card {:no-req true})))
    (testing "advancement requirement must be met"
      (is (not (flags/can-score? state :corp test-card)))
      (is (flags/can-score? state :corp test-card-with-counters)))
    (testing "can-core-req passes in state"
      (defmethod core/defcard-impl "Test Card" [_]
        {:flags {:can-score (req (= (:title card) "Different Card"))}})
      (is (not (flags/can-score? state :corp test-card-with-counters)))
      (is (flags/can-score? state :corp (assoc test-card-with-counters :title "Different Card")))))
  (remove-method core/defcard-impl "Test Card"))
