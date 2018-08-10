(ns game-test.cards.assets.cybernetics-court
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cybernetics-court
  ;; Cybernetics Court
  (do-game
    (new-game {:corp {:deck ["Cybernetics Court"]}})
    (play-from-hand state :corp "Cybernetics Court" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 9 (get-hand-size :corp)) "Corp should have hand size of 9")))
