(ns game-test.cards.operations.surveillance-sweep
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest surveillance-sweep
  ;; Surveillance Sweep
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Restructured Datapool" "Surveillance Sweep" "Data Raven"]}
                 :runner {:deck ["Scrubbed"]}})
      (is (zero? (:tag (get-runner))) "Runner should start with no tags")
      (play-from-hand state :corp "Surveillance Sweep")
      (play-and-score state "Restructured Datapool")
      (let [rd-scored (get-scored state :corp 0)]
        (card-ability state :corp rd-scored 0)
        (is (not= :waiting (-> (get-corp) :prompt first :prompt-type)) "Surveillance Sweep only works during a run")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (:tag (get-runner))) "Runner should gain a tag from Restructured Datapool ability"))
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Data Raven" "HQ")
      (take-credits state :corp)
      (let [dr (get-ice state :hq 0)]
        (core/rez state :corp (refresh dr))
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :runner :waiting) "Runner should now be waiting on Corp")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state))))
  (testing "trace during run after stealing an agenda"
    (do-game
      (new-game {:corp {:deck ["Surveillance Sweep" "Breaking News" "Forced Connection" "Data Raven"]}})
      (core/gain state :corp :click 4)
      (core/gain state :corp :credit 20)
      (play-from-hand state :corp "Surveillance Sweep")
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Forced Connection" "Server 1")
      (play-from-hand state :corp "Data Raven" "Server 1")
      (take-credits state :corp)
      (let [dr (get-ice state :remote1 0)
            bn (get-content state :remote1 0)
            fc (get-content state :remote1 1)]
        (core/rez state :corp (refresh dr))
        (run-on state :remote1)
        (card-subroutine state :corp dr 0)
        (is (prompt-is-type? state :corp :waiting) "During a run, Corp should wait on Runner first")
        (click-prompt state :runner "0")
        (click-prompt state :corp "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (click-card state :runner bn)
        (click-prompt state :runner "Steal")
        (click-card state :runner fc)
        (is (prompt-is-type? state :runner :waiting) "After steal, Surveillance Sweep leaves play and Runner waits on Corp")))))
