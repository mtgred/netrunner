(ns game-test.cards.events.white-hat
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest white-hat
  ;; White Hat
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Fire Wall" "Enigma"]}
               :runner {:deck ["White Hat"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (play-from-hand state :runner "White Hat")
    (is (= :waiting (-> (get-runner) :prompt first :prompt-type)) "Runner is waiting for Corp to boost")
    (click-prompt state :corp "0")
    (click-prompt state :runner "4")
    (click-prompt state :runner (find-card "Ice Wall" (:hand (get-corp))))
    (click-prompt state :runner (find-card "Enigma" (:hand (get-corp))))
    (is (= #{"Ice Wall" "Enigma"} (->> (get-corp) :deck (map :title) (into #{}))))))
