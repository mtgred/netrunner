(ns game-test.cards.agendas.voting-machine-initiative
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest voting-machine-initiative
  ;; Voting Machine Initiative
  (testing "Voting Machine Initiative"
    (do-game
      (new-game {:corp {:deck ["Voting Machine Initiative"]}})
      (letfn [(vmi-test [vmi choice counter]
                (let [diff (if (= "Yes" choice) 1 0)]
                  (is (= counter (get-counters (refresh vmi) :agenda)))
                  (is (= 4 (:click (get-runner))))
                  (click-prompt state :corp choice)
                  (is (= (- 4 diff) (:click (get-runner))))
                  (is (= (- counter diff) (get-counters (refresh vmi) :agenda)))
                  (take-credits state :runner)
                  (take-credits state :corp)))]
        (play-and-score state "Voting Machine Initiative")
        (take-credits state :corp)
        (let [vmi-scored (get-scored state :corp 0)]
          (vmi-test vmi-scored "Yes" 3)
          (vmi-test vmi-scored "No" 2)
          (vmi-test vmi-scored "Yes" 2)
          (vmi-test vmi-scored "Yes" 1)
          (is (empty (:prompt (get-corp))) "No prompt as there are no agenda counters left"))))))
