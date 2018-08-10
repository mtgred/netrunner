(ns game-test.cards.hardware.comet
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest comet
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game {:runner {:deck [(qty "Comet" 3) (qty "Easy Mark" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-hardware state 0)]
      (play-from-hand state :runner "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :runner comet 0)
      (is (= (:cid comet) (-> @state :runner :prompt first :card :cid)))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))
