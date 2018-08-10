(ns game-test.cards.events.test-run
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest test-run
  ;; Test Run
  (testing "Programs hosted after install get returned to Stack. Issue #1081"
    (do-game
      (new-game {:corp {:deck ["Wraparound"]}}
                {:runner {:deck [(qty "Test Run" 2) "Morning Star"
                                 "Knight" "Leprechaun"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap)
        (take-credits state :corp)
        (core/gain state :runner :credit 5)
        (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
        (core/move state :runner (find-card "Knight" (:hand (get-runner))) :discard)
        (let [ms (find-card "Morning Star" (:discard (get-runner)))]
          (play-from-hand state :runner "Leprechaun")
          (play-from-hand state :runner "Test Run")
          (click-prompt state :runner "Heap")
          (click-prompt state :runner ms)
          (let [lep (get-program state 0)
                ms (get-program state 1)]
            (card-ability state :runner lep 1)
            (click-card state :runner ms)
            (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep")
            (take-credits state :runner)
            (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
            (take-credits state :corp)
            (let [kn (find-card "Knight" (:discard (get-runner)))]
              (play-from-hand state :runner "Test Run")
              (click-prompt state :runner "Heap")
              (click-prompt state :runner kn)
              (let [kn (get-program state 1)]
                (card-ability state :runner kn 0)
                (click-card state :runner wrap)
                (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
                (take-credits state :runner)
                (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ICE"))))))))
  (testing "Make sure program remains installed if Scavenged"
    (do-game
      (new-game {:runner {:deck ["Test Run" "Morning Star"
                                 "Scavenge" "Inti"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Test Run")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms)
        (is (= 2 (:credit (get-runner))) "Program installed for free")
        (let [ms (get-program state 0)]
          (play-from-hand state :runner "Scavenge")
          (click-card state :runner ms)
          (click-card state :runner (find-card "Morning Star" (:discard (get-runner))))
          (take-credits state :runner)
          (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
          (is (= "Morning Star" (:title (get-program state 0))) "Morning Star still installed"))))))

