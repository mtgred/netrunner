(ns game-test.cards.agendas.water-monopoly
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest water-monopoly
  ;; Water Monopoly
  (do-game
    (new-game {:corp {:deck ["Water Monopoly"]}
               :runner {:deck ["Fan Site" "Levy Advanced Research Lab"]}})
    (play-and-score state "Water Monopoly")
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner should start with 5 credits")
    (play-from-hand state :runner "Fan Site")
    (is (= 5 (:credit (get-runner))) "Shouldn't lose any credits")
    (play-from-hand state :runner "Levy Advanced Research Lab")
    (is (zero? (:credit (get-runner))) "Should cost an extra credit to play")))
