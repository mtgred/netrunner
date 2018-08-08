(ns game-test.cards.assets.shattered-remains
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shattered-remains
  ;; Shattered Remains
  (do-game
    (new-game {:corp {:deck [(qty "Shattered Remains" 2)]}
               :runner {:deck ["Cyberfeeder" "Lemuria Codecracker"]}})
    (play-from-hand state :corp "Shattered Remains" "New remote")
    (play-from-hand state :corp "Shattered Remains" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Cyberfeeder")
    (play-from-hand state :runner "Lemuria Codecracker")
    (take-credits state :runner)
    (let [remains1 (get-content state :remote1 0)
          remains2 (get-content state :remote2 0)
          cyber (get-hardware state 0)
          lemuria (get-hardware state 1)]
      (core/rez state :corp remains1)
      (advance state remains2 1)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get Shattered Remains ability prompt when no counters")
      (click-prompt state :runner "No action")
      (run-empty-server state :remote2)
      (let [credits (:credit (get-corp))]
        (click-prompt state :corp "Yes")
        (click-card state :corp cyber)
        (is (= (- credits 1) (:credit (get-corp))) "Shattered Remains ability should cost 1")
        (is (count (:discard (get-runner))) "Cyberfeeder should be in discard from Shattered Remains")))))
