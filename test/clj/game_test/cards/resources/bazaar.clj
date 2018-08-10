(ns game-test.cards.resources.bazaar
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bazaar
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
    (new-game {:runner {:deck ["Street Peddler"
                               "Bazaar"
                               (qty "Spy Camera" 6)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
    (play-from-hand state :runner "Bazaar")
    (play-from-hand state :runner "Street Peddler")
    (let [peddler (get-resource state 1)]
      (card-ability state :runner peddler 0)
      (click-prompt state :runner (first (:hosted peddler)))
      (is (empty? (:prompt (get-runner))) "No Bazaar prompt from install off Peddler"))))
