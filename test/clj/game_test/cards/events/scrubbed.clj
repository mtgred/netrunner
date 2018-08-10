(ns game-test.cards.events.scrubbed
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest scrubbed
  ;; First piece of ice encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game {:corp {:deck ["Turing"]}
               :runner {:deck ["Street Peddler"
                               (qty "Scrubbed" 3)]}})
    (starting-hand state :runner ["Street Peddler" "Scrubbed"])
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (let [turing (get-ice state :hq 0)]
      (core/rez state :corp turing)
      (is (= 2 (:current-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:current-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (play-from-hand state :runner "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (:current-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-successful state))))
