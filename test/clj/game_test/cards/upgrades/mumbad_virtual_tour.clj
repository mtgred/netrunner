(ns game-test.cards.upgrades.mumbad-virtual-tour
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mumbad-virtual-tour
  ;; Tests that Mumbad Virtual Tour forces trash when no :slow-trash
  (do-game
    (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)]}})
    (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    ;; MVT does not force trash when not installed
    (click-prompt state :runner "No action")
    (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT in HQ")
    (is (empty? (:discard (get-corp))) "MVT in HQ is not trashed")
    (run-empty-server state "Server 1")
    (is (= 1 (-> @state :runner :prompt first :choices count)) "Should only have a single option")
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (zero? (:credit (get-runner))) "Runner forced to trash MVT")
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed"))
  (testing "interaction with Imp"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)]}
                 :runner {:deck ["Imp"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      ;; Reset credits to 5
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      ;; Runner not force to trash since Imp is installed
      (is (= 2 (-> @state :runner :prompt first :choices count)) "Runner has 2 choices when Imp is installed")
      (is (= 5 (:credit (get-runner))) "Runner not forced to trash MVT when Imp installed")
      (is (empty? (:discard (get-corp))) "MVT is not force-trashed when Imp installed")
      (let [imp (get-program state 0)]
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-corp))))) "MVT trashed with Imp")
        ;; Trash Imp to reset :slow-trash flag
        (core/move state :runner (refresh imp) :discard)
        (is (not (core/any-flag-fn? state :runner :slow-trash true))))))
  (testing "interactions with Imp and various amounts of money"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:deck ["Imp"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (is (= 3 (:credit (get-runner))) "Runner paid install costs")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 1")
      (is (= #{"[Imp]: Trash card" "Pay 5 [Credits] to trash"}
             (->> (get-runner) :prompt first :choices (into #{}))) "Should have Imp and MVT options")
      (click-prompt state :runner "[Imp]: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 2")
      (is (= ["[Imp]: Trash card"] (-> (get-runner) :prompt first :choices)) "Should only have Imp option")
      (click-prompt state :runner "[Imp]: Trash card")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner)))
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 3")
      (is (= ["No action"] (-> (get-runner) :prompt first :choices)) "Should only have no action option")
      (click-prompt state :runner "No action")
      (is (= 2 (->> (get-corp) :discard count)) "Runner was not forced to trash MVT")))
  (testing "not forced to trash when credits below 5"
    (do-game
      (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 3)]}
                 :runner {:deck ["Cache"]}})
      (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (is (= 4 (:credit (get-runner))) "Runner paid install costs")
      (run-empty-server state "Server 1")
      (is (= ["No action"] (-> (get-runner) :prompt first :choices)) "Can't trash"))))
