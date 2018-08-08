(ns game-test.cards.assets.aggressive-secretary
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest aggressive-secretary
  ;; Aggressive Secretary
  (do-game
    (new-game {:corp {:deck ["Aggressive Secretary"]}
               :runner {:deck [(qty "Cache" 3)]}})
    (play-from-hand state :corp "Aggressive Secretary" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (core/advance state :corp {:card (refresh as)})
      (take-credits state :corp)
      ;; Run on AggSec with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes")
      (is (= 3 (:credit (get-corp))))
      ;; Corp can trash one program
      (click-card state :corp (get-program state 1))
      ;; There should be two Caches left
      (is (= 3 (:credit (get-corp))))
      (is (= 2 (count (get-program state)))))))
