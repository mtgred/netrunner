(ns game-test.cards.resources.power-tap
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest power-tap
  ;; Power Tap
  (do-game
    (new-game {:corp {:deck ["Restructured Datapool"]}
               :runner {:deck ["Power Tap"]}})
    (play-and-score state "Restructured Datapool")
    (let [agenda (get-scored state :corp 0)
          tags (:tag (get-runner))
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= credits (:credit (get-runner))) "Runner shouldn't gain any credits from trace")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= (+ tags 1) (:tag (get-runner))) "Runner should gain 1 tag from losing trace"))
    (take-credits state :corp)
    (play-from-hand state :runner "Power Tap")
    (take-credits state :runner)
    (let [agenda (get-scored state :corp 0)
          tags (:tag (get-runner))
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= (+ credits 1) (:credit (get-runner))) "Runner should gain 1 credit from trace initiation")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= (+ tags 1) (:tag (get-runner))) "Runner should gain 1 tag from losing trace"))))
