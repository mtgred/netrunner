(ns game.core.mark-test
  (:require [game.core :as core]
            [game.core.eid :as eid]
            [game.core.sabotage :as s]
            [game.core.mark :as m]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

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
