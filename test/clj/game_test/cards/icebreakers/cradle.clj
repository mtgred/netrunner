(ns game-test.cards.icebreakers.cradle
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cradle
  ;; Cradle
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Cradle" (qty "Cache" 100)]}})
    (starting-hand state :runner ["Cradle"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 100 :click 100)
    (play-from-hand state :runner "Cradle")
    (run-on state "HQ")
    (let [cradle (get-program state 0)
          strength (:strength (refresh cradle))]
      (dotimes [n 5]
        (when (pos? n)
          (core/draw state :runner n))
        (is (= (- strength n) (:current-strength (refresh cradle))) (str "Cradle should lose " n " strength"))
        (starting-hand state :runner [])
        (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))
      (core/draw state :runner 1)
      (is (= (dec strength) (:current-strength (refresh cradle))) "Cradle should lose 1 strength")
      (play-from-hand state :runner "Cache")
      (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))))
