(ns game-test.cards.resources.globalsec-security-clearance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game {:runner {:deck ["Globalsec Security Clearance"]}})
    (take-credits state :corp)
    (core/gain state :runner :link 2)
    (play-from-hand state :runner "Globalsec Security Clearance")
    (take-credits state :runner)
    (starting-hand state :corp ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [gsec (get-resource state 0)]
      (card-ability state :runner gsec 0)
      (is (pos? (.indexOf (-> (get-runner) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :runner nil)
      (is (= 3 (:click (get-runner))) "Runner lost 1 click from Globalsec Security Clearance"))))
