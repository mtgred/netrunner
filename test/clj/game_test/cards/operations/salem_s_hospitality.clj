(ns game-test.cards.operations.salem-s-hospitality
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest salem-s-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Salem's Hospitality" 3)]}
               :runner {:deck [(qty "I've Had Worse" 3) "Faust"
                               "Levy AR Lab Access"]}})
    (play-from-hand state :corp "Salem's Hospitality")
    (is (= 5 (count (:hand (get-runner)))))
    (click-prompt state :corp "I've Had Worse")
    (is (= 2 (count (:hand (get-runner)))))
    (play-from-hand state :corp "Salem's Hospitality")
    (click-prompt state :corp "Plascrete Carapace")
    (is (= 2 (count (:hand (get-runner)))))))
