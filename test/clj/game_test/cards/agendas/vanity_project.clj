(ns game-test.cards.agendas.vanity-project
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest vanity-project
  ;; Vanity Project
  (do-game
    (new-game {:corp {:deck ["Vanity Project"]}})
    (play-and-score state "Vanity Project")
    (is (= 4 (:agenda-point (get-corp))))))
