(ns game-test.cards.agendas.fly-on-the-wall
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fly-on-the-wall
  ;; Fly on the Wall - give the runner 1 tag
  (do-game
    (new-game {:corp {:deck ["Fly on the Wall"]}})
    (is (zero? (:tag (get-runner))) "Runner starts with no tags")
    (play-and-score state "Fly on the Wall")
    (is (= 1 (:tag (get-runner))) "Runner is tagged")))
