(ns game-test.cards.agendas.project-beale
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-beale
  ;; Project Beale
  (do-game
    (new-game {:corp {:deck [(qty "Project Beale" 2)]}})
    (core/gain state :corp :click 8 :credit 8)
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [pb1 (get-content state :remote1 0)]
      (advance state pb1 4)
      (core/score state :corp {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :corp "Project Beale" "New remote"))
    (let [pb2 (get-content state :remote2 0)]
      (advance state pb2 5)
      (core/score state :corp {:card (refresh pb2)})
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))
