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

  (testing "/link"
    (let [user {:username "Runner"}]
      (testing "Can increase link"
        (do-game
          (new-game)
          (changes-val-macro
            1 (:link (get-runner))
            "Link increases by 1"
            (core/say state :runner {:user user :text "/link 1"})))))))
