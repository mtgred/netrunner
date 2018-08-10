(ns game-test.cards.assets.reconstruction-contract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when runner takes meat damage
  (do-game
    (new-game {:corp {:deck ["Reconstruction Contract" "Scorched Earth" "Pup"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Imp" 3)]}})
    (core/gain state :runner :tag 1)
    (core/gain state :corp :credit 5)
    (starting-hand state :runner ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :corp "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :corp (refresh rc))
      (play-from-hand state :corp "Scorched Earth")
      (is (= 4 (count (:discard (get-runner)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :runner ["Imp" "Imp"])
      (play-from-hand state :corp "Pup" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (card-subroutine state :corp (get-ice state :hq 0) 0)
      (is (= 5 (count (:discard (get-runner)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract doesn't get advancement token for net damage"))))
