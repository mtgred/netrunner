(ns game-test.cards.assets.amani-senai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest amani-senai
  ;; Amani Senai - trace on score/steal to bounce, with base strength = advancement req of the agenda
  (do-game
    (new-game {:corp {:deck ["Amani Senai"
                             (qty "Medical Breakthrough" 2)]}
               :runner {:deck ["Analog Dreamers"]}})
    (play-from-hand state :corp "Amani Senai" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (take-credits state :corp)
    (let [senai (get-content state :remote1 0)
          breakthrough (get-content state :remote3 0)]
      (core/rez state :corp senai)
      (play-from-hand state :runner "Analog Dreamers")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Steal")
      (is (zero? (count (get-content state :remote2))) "Agenda was stolen")
      (click-prompt state :corp "Medical Breakthrough") ; simult. effect resolution
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (is (= 3 (-> (get-runner) :prompt first :strength)) "Trace base strength is 3 after stealing first Breakthrough")
      (click-prompt state :runner "0")
      (let [grip (-> (get-runner) :hand count)]
        (is (= 1 (count (get-program state))) "There is an Analog Dreamers installed")
        (click-card state :corp (get-program state 0))
        (is (zero? (count (get-program state))) "Analog Dreamers was uninstalled")
        (is (= (+ grip 1) (-> (get-runner) :hand count)) "Analog Dreamers was added to hand"))
      (take-credits state :runner)
      (score-agenda state :corp breakthrough)
      ;; (click-prompt state :corp "Medical Breakthrough") ; there is no simult. effect resolution on score for some reason
      (click-prompt state :corp "Yes") ; corp should get to trigger trace even when no runner cards are installed
      (click-prompt state :corp "0")
      (is (= 2 (-> (get-runner) :prompt first :strength)) "Trace base strength is 2 after scoring second Breakthrough"))))
