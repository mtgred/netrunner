(ns game-test.cards.agendas.glenn-station
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest glenn-station
  ;; Glenn Station
  (do-game
    (new-game {:corp {:deck ["Glenn Station" "Ice Wall"]}})
    (play-and-score state "Glenn Station")
    (let [gs-scored (get-scored state :corp 0)]
      (card-ability state :corp gs-scored 0)
      (click-prompt state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= 1 (count (:hosted (refresh gs-scored)))))
      (card-ability state :corp gs-scored 1)
      (click-prompt state :corp (find-card "Ice Wall" (:hosted (refresh gs-scored))))
      (is (zero? (count (:hosted (refresh gs-scored))))))))
