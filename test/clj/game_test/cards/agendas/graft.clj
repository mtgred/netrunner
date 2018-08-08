(ns game-test.cards.agendas.graft
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest graft
  ;; Graft
  (letfn [(graft-test [[number-of-picks deck-size]]
            (let [cards ["Ice Wall" "Fire Wall" "Orion"]]
              (do-game
                (new-game {:corp {:deck ["Graft" "Ice Wall"
                                         "Fire Wall" "Orion"]}})
                (starting-hand state :corp ["Graft"])
                (play-and-score state "Graft")
                (dotimes [current-pick number-of-picks]
                  (click-prompt state :corp (find-card (nth cards current-pick) (:deck (get-corp)))))
                (is (= number-of-picks (count (:hand (get-corp)))))
                (is (= deck-size (count (:deck (get-corp))))))))]
    (doall (map graft-test
                [[0 3]
                 [1 2]
                 [2 1]
                 [3 0]]))))
