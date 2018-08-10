(ns game-test.cards.agendas.profiteering
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest profiteering
  ;; Profiteering
  (do-game
    (new-game {:corp {:deck ["Profiteering"]}})
    (play-and-score state "Profiteering")
    (click-prompt state :corp "3")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 3 (:bad-publicity (get-corp))) "Took 3 bad publicity")
    (is (= 20 (:credit (get-corp))) "Gained 15 credits")))
