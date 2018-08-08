(ns game-test.cards.operations.enhanced-login-protocol
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol
  (testing "First click run each turn costs an additional click"
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Employee Strike"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make the first run")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make the second run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner 3)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")
      (run-on state :archives)
      (is (not (:run @state)) "No run was initiated")
      (is (= 1 (:click (get-runner))) "Runner has 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run")))
  (testing "Card ability runs don't cost additional clicks"
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 2 clicks")
      (let [sneakdoor (get-program state 0)]
        (card-ability state :runner sneakdoor 0)
        (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a card ability")
        (run-successful state)
        (run-on state :archives)
        (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (run-on state :archives)
        (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run"))))
  (testing "with New Angeles Sol, Enhanced Login Protocol trashed and reinstalled on steal doesn't double remove penalty"
    (do-game
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Enhanced Login Protocol" "Breaking News"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enhanced Login Protocol" (:discard (get-corp))))
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner has 1 click")))
  (testing "Run event don't cost additional clicks"
    (do-game
      (new-game {:corp {:deck ["Enhanced Login Protocol"]}
                 :runner {:deck ["Out of the Ashes"]}})
      (play-from-hand state :corp "Enhanced Login Protocol")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (play-from-hand state :runner "Out of the Ashes")
      (click-prompt state :runner "Archives")
      (is (= 3 (:click (get-runner))) "Runner doesn't spend 1 additional click to run with a run event")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "No") ; Out of the Ashes prompt
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
  (testing "Works when played on the runner's turn"
    (do-game
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Enhanced Login Protocol"
                               "Breaking News"]}
                 :runner {:deck ["Hades Shard"]}})
      (trash-from-hand state :corp "Breaking News")
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Hades Shard")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
      (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks")
      (run-on state :archives)
      (is (= 1 (:click (get-runner))) "Runner spends 1 additional click to make a run")))
(testing "Doesn't fire if already run when played on the runner's turn"
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck ["Enhanced Login Protocol"
                             "Breaking News"]}
               :runner {:deck ["Hades Shard"]}})
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :runner "No action")
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Enhanced Login Protocol" (:hand (get-corp))))
    (is (find-card "Enhanced Login Protocol" (:current (get-corp))) "Enhanced Login Protocol is in play")
    (is (= 2 (:click (get-runner))) "Runner has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-runner))) "Runner doesn't spend 1 additional click to make a run"))))
