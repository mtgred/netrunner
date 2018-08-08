(ns game-test.cards.assets.ngo-front
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ngo-front
  ;; NGO Front - full test
  (do-game
    (new-game {:corp {:deck [(qty "NGO Front" 3)]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (let [ngo1 (get-content state :remote1 0)
          ngo2 (get-content state :remote2 0)
          ngo3 (get-content state :remote3 0)]
      (core/advance state :corp {:card ngo2})
      (core/advance state :corp {:card (refresh ngo3)})
      (core/advance state :corp {:card (refresh ngo3)})
      (core/rez state :corp (refresh ngo1))
      (core/rez state :corp (refresh ngo2))
      (core/rez state :corp (refresh ngo3))
      (is (= 2 (:credit (get-corp))) "Corp at 2 credits")
      (card-ability state :corp ngo1 1)
      (card-ability state :corp ngo1 0)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :corp ngo2 1)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :corp ngo2 0)
      (is (= 7 (:credit (get-corp))) "Corp gained 5 credits")
      (is (= 1 (count (:discard (get-corp)))) "1 NGO Front Trashed")
      (card-ability state :corp ngo3 1)
      (is (= 15 (:credit (get-corp))) "Corp gained 8 credits")
      (is (= 2 (count (:discard (get-corp)))) "2 NGO Front Trashed"))))
