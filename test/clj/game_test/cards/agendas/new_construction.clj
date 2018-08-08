(ns game-test.cards.agendas.new-construction
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest new-construction
  ;; New Construction
  (do-game
    (new-game {:corp {:deck ["New Construction" (qty "Commercial Bankers Group" 10)]}})
    (starting-hand state :corp (vec (cons "New Construction" (repeat 10 "Commercial Bankers Group"))))
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "New Construction" "New remote")
    (let [nc (get-content state :remote1 0)]
      (is (zero? (get-counters (refresh nc) :advancement)))
      (dotimes [n 4]
        (advance state (refresh nc))
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Commercial Bankers Group" (:hand (get-corp)))))
      (is (= 4 (get-counters (refresh nc) :advancement)))
      (is (not= :this-turn (:rezzed (get-content state :remote5 0))))
      (let [credits (:credit (get-corp))]
        (advance state (refresh nc))
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Commercial Bankers Group" (:hand (get-corp))))
        (is (= 5 (get-counters (refresh nc) :advancement)))
        (is (= :this-turn (:rezzed (get-content state :remote6 0))))
        (is (= (dec credits) (:credit (get-corp))))))))
