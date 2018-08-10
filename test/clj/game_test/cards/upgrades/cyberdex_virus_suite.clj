(ns game-test.cards.upgrades.cyberdex-virus-suite
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cyberdex-virus-suite
  ;; Cyberdex Virus Suite
  (testing "Purge ability"
    (do-game
      (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
                 :runner {:deck ["Cache" "Medium"]}})
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state :runner (refresh card)))
            cache (find-card "Cache" (get-program state))
            cvs (get-content state :hq 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (take-credits state :runner 2)
        (core/rez state :corp cvs)
        (card-ability state :corp cvs 0)
        ;; nothing in hq content
        (is (empty? (get-content state :hq)) "CVS was trashed")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))
  (testing "Purge on access"
    (do-game
      (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
                 :runner {:deck ["Cache" "Medium"]}})
      (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
      (take-credits state :corp 2)
      ;; runner's turn
      ;; install cache and medium
      (play-from-hand state :runner "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state :runner (refresh card)))
            cache (find-card "Cache" (get-program state))
            cvs (get-content state :remote1 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :runner "Medium")
        (run-empty-server state "Server 1")
        ;; corp now has optional prompt to trigger virus purge
        (click-prompt state :corp "Yes")
        ;; runner has prompt to trash CVS
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-program state))))
            "Medium has no counters"))))
  (testing "Don't interrupt archives access, #1647"
    (do-game
      (new-game {:corp {:deck ["Cyberdex Virus Suite" "Braintrust"]}
                 :runner {:deck ["Cache"]}})
      (trash-from-hand state :corp "Cyberdex Virus Suite")
      (trash-from-hand state :corp "Braintrust")
      (take-credits state :corp)
      ;; runner's turn
      ;; install cache
      (play-from-hand state :runner "Cache")
      (let [cache (get-program state 0)]
        (is (= 3 (get-counters (refresh cache) :virus)))
        (run-empty-server state "Archives")
        (click-prompt state :runner "Cyberdex Virus Suite")
        (click-prompt state :corp "Yes")
        (is (pos? (count (:prompt (get-runner)))) "CVS purge did not interrupt archives access")
        ;; purged counters
        (is (zero? (get-counters (refresh cache) :virus))
            "Cache has no counters")))))
