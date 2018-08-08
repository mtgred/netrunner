(ns game-test.cards.upgrades.mumbad-city-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mumbad-city-grid
  ;; Mumbad City Grid - when runner passes a piece of ice, swap that ice with another from this server
  (testing "1 ice"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Grid" "Quandary"]}})
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (let [mcg (get-content state :remote1 0)]
        (core/rez state :corp mcg)
        (take-credits state :corp)
        (run-on state "Server 1")
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "1 ice on server")
        (card-ability state :corp (refresh mcg) 0)
        (run-continue state)
        (card-ability state :corp (refresh mcg) 0)
        (run-jack-out state)
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 1 ice on server"))))
  (testing "fire before pass"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Grid" "Quandary" "Ice Wall"]}})
      (play-from-hand state :corp "Mumbad City Grid" "New remote")
      (play-from-hand state :corp "Quandary" "Server 1")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (let [mcg (get-content state :remote1 0)]
        (core/rez state :corp mcg)
        (take-credits state :corp)
        (run-on state "Server 1")
        (is (= 2 (:position (:run @state))) "Runner at position 2")
        (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "2 ice on server")
        (is (= "Quandary" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Quandary inner ice")
        (is (= "Ice Wall" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall outer ice")
        (card-ability state :corp (refresh mcg) 0)
        (run-continue state)
        (is (= 1 (:position (:run @state))) "Runner at position 1")
        (card-ability state :corp (refresh mcg) 0)
        (click-card state :corp (get-ice state :remote1 0))
        (is (= 1 (:position (:run @state))) "Runner at position 1")
        (is (= "Quandary" (:title (second (get-in @state [:corp :servers :remote1 :ices])))) "Quandary outer ice")
        (is (= "Ice Wall" (:title (first (get-in @state [:corp :servers :remote1 :ices])))) "Ice Wall inner ice")
        (run-jack-out state)
        (is (= 2 (count (get-in @state [:corp :servers :remote1 :ices]))) "Still 2 ice on server")))))
