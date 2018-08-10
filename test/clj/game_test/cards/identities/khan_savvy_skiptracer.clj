(ns game-test.cards.identities.khan-savvy-skiptracer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest khan-savvy-skiptracer
  ;; Khan
  (testing "proper order of events when vs. Caprice"
    (do-game
      (new-game {:corp {:deck ["Eli 1.0" "Caprice Nisei"]}
                 :runner {:id "Khan: Savvy Skiptracer"
                          :deck ["Corroder"]}})
      (play-from-hand state :corp "Eli 1.0" "Archives")
      (play-from-hand state :corp "Caprice Nisei" "Archives")
      (core/rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (is (and (empty? (:prompt (get-corp)))
               (= 1 (count (:prompt (get-runner))))
               (= "Khan: Savvy Skiptracer" (-> (get-runner) :prompt first :card :title)))
          "Only Khan prompt showing")
      (click-card state :runner (first (:hand (get-runner))))
      (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
      (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
      (is (= "Caprice Nisei" (-> (get-runner) :prompt first :card :title)) "Caprice prompt showing")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))
