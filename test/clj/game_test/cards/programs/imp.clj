(ns game-test.cards.programs.imp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest imp
  ;; Imp
  (testing "Full test"
    (letfn [(imp-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:deck ["Imp"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Imp")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Imp]: Trash card")
                (is (= 1 (count (:discard (get-corp)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Ice Wall"
                   "Oberth Protocol"]))))
  (testing "vs an ambush"
    (do-game
      (new-game {:corp {:deck ["Prisec"]}
                 :runner {:deck ["Imp" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            tags (:tag (get-runner))
            grip (count (:hand (get-runner)))
            archives (count (:discard (get-corp)))]
        (play-from-hand state :runner "Imp")
        (run-empty-server state :remote1)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= 2 (- credits (:credit (get-corp)))) "Corp paid 2 for Prisec")
        (is (= 1 (- (:tag (get-runner)) tags)) "Runner has 1 tag")
        (is (= 2 (- grip (count (:hand (get-runner))))) "Runner took 1 meat damage")
        (is (= 1 (- (count (:discard (get-corp))) archives)) "Used Imp to trash Prisec"))))
  (testing "vs The Future Perfect"
    ;; Psi-game happens on access [5.5.1], Imp is a trash ability [5.5.2]
    (do-game
      (new-game {:corp {:deck ["The Future Perfect"]}
                 :runner {:deck ["Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (testing "Access, corp wins psi-game"
        (run-empty-server state "HQ")
        ;; Should access TFP at this point
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (click-prompt state :runner "[Imp]: Trash card")
        (take-credits state :runner)
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP")
        (core/move state :corp (find-card "The Future Perfect" (:discard (get-corp))) :hand))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Access, runner wins psi-game"
        (run-empty-server state "HQ")
        ;; Access prompt for TFP
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        ;; Fail psi game
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))))
  (testing "vs cards in Archives"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Imp"]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :discard)
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (run-empty-server state "Archives")
      (is (= ["Steal"] (->> (get-runner) :prompt first :choices)) "Should only get the option to steal Hostile on access in Archives"))))
