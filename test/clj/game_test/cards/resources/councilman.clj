(ns game-test.cards.resources.councilman
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest councilman
  ;; Councilman reverses the rezz and prevents re-rezz
  (do-game
    (new-game {:corp {:deck ["Jackson Howard"]}
               :runner {:deck ["Councilman"]}})
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Councilman")
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :corp jesus)
      ;; Runner triggers Councilman
      (card-ability state :runner judas 0)
      (click-card state :runner jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/rez state :corp (refresh jesus))
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard cannot be rezzed")
      (take-credits state :runner)
      ;; Next turn
      (core/rez state :corp (refresh jesus))
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed next turn"))))
(deftest-pending councilman-zone-change
  ;; Rezz no longer prevented when card changes zone (issues #1571)
  (do-game
    (new-game {:corp {:deck ["Jackson Howard"]}
               :runner {:deck ["Councilman"]}})
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Councilman")
    (take-credits state :runner)
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :corp jesus)
      ;; Runner triggers Councilman
      (card-ability state :runner judas 0)
      (click-card state :runner jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/move state :corp (refresh jesus) :hand))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jesus (get-content state :remote2 0)]
      (core/rez state :corp jesus)
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed after changing zone"))))
