(ns game-test.cards.ice.architect
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest architect
  (testing "Architect is untrashable while installed and rezzed, but trashable if derezzed or from HQ"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3)]}})
      (play-from-hand state :corp "Architect" "HQ")
      (let [architect (get-ice state :hq 0)]
        (core/rez state :corp architect)
        (core/trash state :corp (refresh architect))
        (is (not= nil (get-ice state :hq 0)) "Architect was trashed, but should be untrashable")
        (core/derez state :corp (refresh architect))
        (core/trash state :corp (refresh architect))
        (is (= nil (get-ice state :hq 0)) "Architect was not trashed, but should be trashable")
        (core/trash state :corp (get-in @state [:corp :hand 0]))
        (is (= (get-in @state [:corp :discard 0 :title]) "Architect"))
        (is (= (get-in @state [:corp :discard 1 :title]) "Architect"))))))
