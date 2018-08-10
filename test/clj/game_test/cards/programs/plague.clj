(ns game-test.cards.programs.plague
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest plague
  ;; Plague
  (do-game
    (new-game {:corp {:deck ["Mark Yale"]}
               :runner {:deck ["Plague"]}})
    (play-from-hand state :corp "Mark Yale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Plague")
    (click-prompt state :runner "Server 1")
    (let [plague (get-program state 0)]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))
