(ns game-test.cards.events.by-any-means
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest by-any-means
  ;; By Any Means
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Ice Wall" "Paper Trail" "PAD Campaign"
                               "Project Junebug"]}
                 :runner {:deck ["By Any Means" (qty "Sure Gamble" 5)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      ; (play-from-hand state :runner "By Any Means")
      (is (= 3 (:click (get-runner))) "Card not played, priority restriction")
      (take-credits state :runner)
      (starting-hand state :corp ["Paper Trail" "Hedge Fund" "PAD Campaign" "Project Junebug"])
      (play-from-hand state :corp "Paper Trail" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Project Junebug" "New remote")
      (core/add-counter state :corp (get-content state :remote3 0) :advancement 2)
      (take-credits state :corp)
      (core/gain state :runner :click 2)
      (core/draw state :runner)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
      (is (= 4 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "R&D")
      (is (= 2 (count (:discard (get-corp)))) "ICE was trashed")
      (is (= 3 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 1")
      (is (= 3 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (= 2 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 2")
      (is (= 4 (count (:discard (get-corp)))) "Trashable was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 3")
      (is (= 5 (count (:discard (get-corp)))) "Ambush was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))
  (testing "vs Controlling the Message"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Paper Trail"]}
                 :runner {:deck [(qty "By Any Means" 2)]}})
      (play-from-hand state :corp "Paper Trail" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "No") ;; Don't trigger CTM trace
      (is (empty? (:prompt (get-runner))) "No prompt to steal since agenda was trashed")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))
  (testing "alongside Film Critic: should get the option to trigger either"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
                 :runner {:deck ["By Any Means" "Film Critic" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (play-from-hand state :runner "Film Critic")
      (is (= 1 (count (:discard (get-runner)))) "By Any Means has been played")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-runner) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :runner "Film Critic")
      (click-prompt state :runner "No")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/move state :runner (find-card "By Any Means" (:discard (get-runner))) :hand)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-runner) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :runner "By Any Means")
      (is (nil? (->> (get-runner) :prompt first :choices)) "By Any Means trashes with no prompt")
      (is (= 2 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage"))))
