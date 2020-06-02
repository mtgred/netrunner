(ns game.core.io-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest chat-commands

  (testing "/end-run"
    (let [user {:username "Corp"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/say state :corp {:user user :text "/end-run"})
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (core/say state :corp {:user user :text "/end-run"})
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Runner"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/say state :runner {:user user :text "/end-run"})
          (is (:run @state) "Run is still active")))))

  (testing "/jack-out"
    (let [user {:username "Runner"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/say state :runner {:user user :text "/jack-out"})
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (core/say state :runner {:user user :text "/jack-out"})
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Corp"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/say state :corp {:user user :text "/jack-out"})
          (is (:run @state) "Run is still active")))))

  (testing "/discard #n"
    (let [user {:username "Runner"}]
      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand ["Cache"]}})
          (take-credits state :corp)
          (is (= ["Cache"] (->> (get-runner) :hand (mapv :title))) "Cache should be in hand")
          (core/say state :runner {:user user :text "/discard #1"})
          (is (empty? (:hand (get-runner))) "Runner has empty grip")))))

  (testing "/summon"
    (let [user {:username "Runner"}]
      (testing "Add card with short title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/say state :runner {:user user :text "/summon DDoS"})
          (is (= ["DDoS"] (->> (get-runner) :hand (mapv :title))) "DDoS should now be added into hand")))

      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/say state :runner {:user user :text "/summon Harmony AR Therapy"})
          (is (= ["Harmony AR Therapy"] (->> (get-runner) :hand (mapv :title))) "Harmony AR Therapy should now be added into hand")))))

  (testing "/link"
    (let [user {:username "Runner"}]
      (testing "Can increase link"
        (do-game
          (new-game)
          (changes-val-macro
            1 (:link (get-runner))
            "Link increases by 1"
            (core/say state :runner {:user user :text "/link 1"}))))))

  (testing "/unique"
    (let [user {:username "Runner"}]
      (testing "Works with Wireless Net Pavillion"
        (do-game
          (new-game {:runner {:hand [(qty "Wireless Net Pavilion" 2)]}})
          (take-credits state :corp)
          (let [wnp1 (nth (:hand (get-runner)) 0)
                wnp2 (nth (:hand (get-runner)) 1)]
            (core/play state :runner {:card wnp1})
            (core/play state :runner {:card wnp2})
            (is (= 1 (count (:hand (get-runner)))) "Second WNP was not installed")
            (is (:uniqueness wnp2) "WNP is unique")
            (core/say state :runner {:user user :text "/unique"})
            (is (last-log-contains? state "\\[!\\]Runner uses a command: /unique") "Correct message")
            (click-card state :runner wnp2)
            (is (not (:uniqueness (refresh wnp2))) "WNP is not unique anymore")
            (is (last-log-contains? state "Runner uses /unique command to make Wireless Net Pavilion not unique\\.") "Correct message")
            (core/play state :runner {:card wnp2})
            (is (zero? (count (:hand (get-runner)))) "Both cards have been installed")))))))
