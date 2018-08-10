(ns game-test.cards.ice.tithonium
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tithonium
  ;; Forfeit option as rez cost, can have hosted condition counters
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Tithonium" "Patch"]}
                 :runner {:deck ["Pawn" "Wasteland"]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Tithonium" "HQ")
      (let [ht (get-content state :remote1 0)
            ti (get-ice state :hq 0)]
        (score-agenda state :corp ht)
        (is (= 1 (count (:scored (get-corp)))) "Agenda scored")
        (is (= 12 (:credit (get-corp))) "Gained 7 credits")
        (core/rez state :corp ti)
        (click-prompt state :corp "No") ; don't use alternative cost
        (is (= 3 (:credit (get-corp))) "Spent 9 to Rez")
        (core/derez state :corp (refresh ti))
        (core/rez state :corp ti)
        (click-prompt state :corp "Yes") ; use alternative cost
        (click-card state :corp (get-in (get-corp) [:scored 0]))
        (is (= 3 (:credit (get-corp))) "Still on 3c")
        (is (zero? (count (:scored (get-corp)))) "Agenda forfeited")
        ;; Can Host Conditions Counters
        (play-from-hand state :corp "Patch")
        (click-card state :corp (refresh ti))
        (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
        (take-credits state :corp)
        (core/derez state :corp (refresh ti))
        (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
        (play-from-hand state :runner "Pawn")
        (play-from-hand state :runner "Wasteland")
        (let [pawn (get-program state 0)
              wast (get-resource state 0)]
          (card-ability state :runner (refresh pawn) 0)
          (click-card state :runner (refresh ti))
          (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
          (core/derez state :corp (refresh ti))
          (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
          (run-on state "HQ")
          (card-subroutine state :corp ti 2)
          (click-card state :corp (refresh wast))
          (is (= 1 (count (:discard (get-runner)))) "1 card trashed")
          (card-subroutine state :corp ti 1)
          (is (not (:run @state)) "Run ended")))))
  (testing "Do not prompt for alt cost #2734"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Oversight AI" "Tithonium"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Tithonium" "R&D")
      (let [ht (get-content state :remote1 0)
            ti (get-ice state :rd 0)]
        (score-agenda state :corp ht)
        (play-from-hand state :corp "Oversight AI")
        (click-card state :corp ti)
        (is (:rezzed (refresh ti)))
        (is (= "Oversight AI" (:title (first (:hosted (refresh ti)))))
            "Tithonium hosting OAI as a condition")))))
