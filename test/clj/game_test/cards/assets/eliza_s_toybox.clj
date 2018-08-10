(ns game-test.cards.assets.eliza-s-toybox
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest eliza-s-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game {:corp {:deck ["Eliza's Toybox" "Wotan" "Archer"]}})
    (play-from-hand state :corp "Wotan" "R&D")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Eliza's Toybox" "New remote")
    (let [wotan (get-ice state :rd 0)
          archer (get-ice state :hq 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :corp eliza)
      (is (= 1 (:credit (get-corp))))
      (is (zero? (:click (get-corp))) "3 clicks spent")
      (core/gain state :corp :click 6)
      (card-ability state :corp eliza 0)
      (click-card state :corp wotan)
      (is (:rezzed (refresh wotan)))
      (is (= 3 (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits spent")
      (card-ability state :corp eliza 0)
      (click-card state :corp archer)
      (is (:rezzed (refresh archer)))
      (is (zero? (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits or agendas spent"))))
