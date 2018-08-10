(ns game-test.cards.events.office-supplies
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest office-supplies
  ;; Office Supplies
  (letfn [(office-supplies-test [link]
            (do-game
              (new-game {:runner {:deck [(qty "Office Supplies" 2)
                                         (qty "Access to Globalsec" 100)]}})
              (take-credits state :corp)
              (core/gain state :runner :credit 1000 :click link)
              (starting-hand state :runner (concat (repeat 2 "Office Supplies")
                                                   (repeat 4 "Access to Globalsec")))
              (dotimes [_ link]
                (play-from-hand state :runner "Access to Globalsec"))
              (let [credits (:credit (get-runner))]
                (play-from-hand state :runner "Office Supplies")
                (is (= (- credits (- 4 link)) (:credit (get-runner)))))
              (let [credits (:credit (get-runner))]
                (click-prompt state :runner "Gain 4 [Credits]")
                (is (= (+ 4 credits) (:credit (get-runner))) (str "Runner should gain " (utils/quantify link "credit"))))
              (play-from-hand state :runner "Office Supplies")
              (let [grip (-> (get-runner) :hand count)]
                (click-prompt state :runner "Draw 4 cards")
                (is (= (+ 4 grip) (-> (get-runner) :hand count)) "Runner should draw 4 cards"))))]
    (doall (map office-supplies-test (range 5)))))
