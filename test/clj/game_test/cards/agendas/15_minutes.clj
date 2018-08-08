(ns game-test.cards.agendas.15-minutes
  (:require [game.core :as core]
            [jinteki.cards :refer [all-cards]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "15-minutes"}
  fifteen-minutes
  ;; 15 Minutes - check if it works correctly from both sides
  (do-game
    (new-game {:corp {:hand ["15 Minutes"]}}
              {:runner {:hand ["Wyldside"]}})
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :corp)
    ;; use 15 minutes to take it away from runner
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 1 (count (:scored (get-runner)))))
    (let [fifm (first (:scored (get-runner)))]
      (is (= 3 (:click (get-corp))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-runner))))
      (is (zero? (count (:scored (get-runner))))))
    (is (find-card "15 Minutes" (:deck (get-corp)))))
  (do-game
    (new-game {:corp {:hand ["15 Minutes"]}})
    (play-and-score state "15 Minutes")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 1 (count (:scored (get-corp)))))
    (let [fifm (first (:scored (get-corp)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-corp))))
      (is (zero? (count (:scored (get-corp))))))
    (is (find-card "15 Minutes" (:deck (get-corp))))))
