(ns game-test.cards.resources.net-mercur
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
    (new-game {:runner {:deck ["Net Mercur" "Silencer" "Ghost Runner"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 4 :credit 10)
    (play-from-hand state :runner "Silencer")
    (play-from-hand state :runner "Net Mercur")
    (play-from-hand state :runner "Ghost Runner")
    (let [sil (get-hardware state 0)
          nm (get-resource state 0)
          gr (get-resource state 1)]
      (card-ability state :runner gr 0)
      (is (empty? (:prompt (get-runner))) "No Net Mercur prompt from stealth spent outside of run")
      (run-on state :hq)
      (card-ability state :runner sil 0)
      (click-prompt state :runner "Place 1 [Credits]")
      (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur")
      (card-ability state :runner gr 0)
      (is (empty? (:prompt (get-runner))) "No Net Mercur prompt for 2nd stealth in run")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :runner nm 0)
      (is (= "Net Mercur" (:title (:card (first (get-in @state [:runner :prompt]))))) "Net Mercur triggers itself"))))
