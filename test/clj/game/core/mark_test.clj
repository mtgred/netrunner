(ns game.core.mark-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.eid :as eid]
   [game.core.mark :as m]
   [game.core.sabotage :as s]
   [game.test-framework :refer :all]))

(deftest mark-test
  (testing "Identifying a mark"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 15)]}})
      (is (nil? (:mark @state)) "No mark identified")
      (take-credits state :corp)
      (is (nil? (:mark @state)) "Still no mark identified")
      (core/resolve-ability state :runner (eid/make-eid state)
                            m/identify-mark-ability (:identity (get-runner)) nil)
      (is (some? (:mark @state)) "Mark identified")
      (click-card state :corp (first (:hand (get-corp))))
      (click-credit state :runner)
      (core/resolve-ability state :runner (eid/make-eid state)
                            m/identify-mark-ability (:identity (get-runner)) nil)
      (is (not (last-log-contains? state "identified")) "No new mark identified")
      (take-credits state :runner)
      (is (nil? (:mark @state)) "Mark reset at end of turn")
      (core/resolve-ability state :runner (eid/make-eid state)
                            m/identify-mark-ability (:identity (get-runner)) nil)
      (is (some? (:mark @state)) "Mark identified in Corp turn"))))
