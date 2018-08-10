(ns game-test.cards.events.apocalypse
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest apocalypse
  ;; Apocalypse
  (testing "Ensure MU is correct and no duplicate cards in heap"
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck ["Scheherazade" "Corroder" "Hivemind" (qty "Apocalypse" 2)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Scheherazade")
      (let [scheherazade (get-program state 0)]
        (card-ability state :runner scheherazade 0)
        (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "Memory at 3 (-1 from Corroder)"))
      (play-from-hand state :runner "Hivemind")
      (is (= 1 (core/available-mu state)) "Memory at 1 (-1 from Corroder, -2 from Hivemind)")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (is (zero? (count (core/all-active-installed state :runner))) "No active installed runner cards")
      (let [facedowns (filter :facedown (core/all-installed state :runner))
            scheherazade (find-card "Scheherazade" facedowns)
            corroder (find-card "Corroder" facedowns)
            hivemind (find-card "Hivemind" facedowns)]
        (is scheherazade "Scheherazade facedown")
        (is corroder "Corroder facedown")
        (is hivemind "Hivemind facedown")
        (is (= 3 (count facedowns)) "No other cards facedown")
        (is (= corroder (first (:hosted scheherazade))) "Corroder is still hosted on Scheherazade")
        (is (= 1 (get-counters hivemind :virus)) "Hivemind still has a virus counters"))
      (is (find-card "Apocalypse" (:discard (get-runner))) "Apocalypse is in the heap")
      (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap")
      (is (= 4 (core/available-mu state)) "Memory back to 4")))
  (testing "with Full Immersion - no duplicate cards in heap #2606"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio" "Sandburg"
                               "Oaktown Renovation"]}
                 :runner {:deck ["Apocalypse"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Sandburg" (:hand (get-corp))))
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (take-credits state :corp)
        (run-empty-server state "Archives")
        (run-empty-server state "R&D")
        (run-empty-server state "HQ")
        (play-from-hand state :runner "Apocalypse")
        (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
        (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
        (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap"))))
  (testing "with Hostile Infrastructure - should take damage equal to 2x cards on the table"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 2) (qty "Ice Wall" 2)]}
                 :runner {:deck ["Apocalypse" (qty "Sure Gamble" 9)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/rez state :corp (get-content state :remote1 0) {:ignore-cost true})
      (core/rez state :corp (get-content state :remote4 0) {:ignore-cost true})
      (take-credits state :corp)
      (core/draw state :runner 5)
      (is (= 10 (count (:hand (get-runner)))) "Runner has 9 cards in hand")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 4 (count (:discard (get-corp)))) "4 Corp cards in Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner has one card in hand")
      (is (= 9 (count (:discard (get-runner)))) "There are 9 cards in heap")))
  (testing "Turn Runner cards facedown and reduce memory and hand-size gains"
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck ["Logos" "Apocalypse" (qty "Origami" 2)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Logos")
      (is (= 1 (get-in (get-runner) [:hand-size :mod])) "Hand-size increased from Logos")
      (is (= 5 (core/available-mu state)) "Memory increased from Logos")
      (play-from-hand state :runner "Origami")
      (play-from-hand state :runner "Origami")
      (is (= 5 (get-in (get-runner) [:hand-size :mod])) "Hand-size increased from Logos and Origami")
      (is (= 3 (core/available-mu state)) "Memory decreased from Origamis")
      (core/gain state :runner :click 3 :credit 2)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (let [logos (find-card "Logos" (get-in (get-runner) [:rig :facedown]))]
        (is (:facedown (refresh logos)) "Logos is facedown")
        (is (zero? (get-in (get-runner) [:hand-size :mod])) "Hand-size reset with Logos and Origami facedown")
        (is (= 4 (core/available-mu state)) "Memory reset with Logos and Origami facedown"))))
(testing "Turn Runner cards facedown without firing their trash effects"
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
               :runner {:deck [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Tri-maf Contact")
    (core/gain state :runner :click 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Apocalypse")
    (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [tmc (get-runner-facedown state 0)]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-runner))))
          "No meat damage dealt by Tri-maf's leave play effect")
      (core/trash state :runner tmc)
      (is (= 3 (count (:hand (get-runner))))
          "No meat damage dealt by trashing facedown Tri-maf")))))
