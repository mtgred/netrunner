(ns game-test.cards.identities.haas-bioroid-stronger-together
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Stronger Together"
                      :deck ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :corp eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))
