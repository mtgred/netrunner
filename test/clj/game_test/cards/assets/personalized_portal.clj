(ns game-test.cards.assets.personalized-portal
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest personalized-portal
  ;; Personalized Portal - on corp turn start, force the runner to draw 1 card
  ;; and then gain 1 credit for every 2 cards in the runners hand
  (do-game
    (new-game {:corp {:deck ["Personalized Portal"]}
               :runner {:deck [(qty "Daily Casts" 3) (qty "Dyson Mem Chip" 3)]}})
    (play-from-hand state :corp "Personalized Portal" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (starting-hand state :runner [])
    (is (empty? (:hand (get-runner))) "Runner's grip is empty to start")
    (is (= 4 (:credit (get-corp))) "Corp starts with 4 credits")
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 4 (:credit (get-corp))) "Corp gained 0 credits")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 8 (:credit (get-corp))) "Corp gained 1 credit")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Runner drew 1 card")
    (is (= 12 (:credit (get-corp))) "Corp gained 1 credit")))
