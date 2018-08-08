(ns game-test.cards.operations.service-outage
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest service-outage
  ;; Service Outage
  (testing "First click run each turn costs a credit"
    (do-game
      (new-game {:corp {:deck ["Service Outage"]}
                 :runner {:deck ["Employee Strike"]}})
      (play-from-hand state :corp "Service Outage")
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
      (run-on state :archives)
      (is (= 4 (:credit (get-runner)))
          "Runner spends 1 credit to make the first run")
      (run-successful state)
      (run-on state :archives)
      (is (= 4 (:credit (get-runner)))
          "Runner doesn't spend 1 credit to make the second run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/lose state :runner :credit 6)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (zero? (:credit (get-runner))) "Runner has 0 credits")
      (run-on state :archives)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (zero? (:credit (get-runner))) "Runner has 0 credits")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/lose state :runner :credit 2)
      (play-from-hand state :runner "Employee Strike")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (run-on state :archives)
      (is (= 1 (:credit (get-runner)))
          "Runner doesn't spend 1 credit to make a run")))
  (testing "First card ability run each turn costs an additional credit"
    (do-game
      (new-game {:corp {:deck ["Service Outage"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Service Outage")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (take-credits state :runner 1)
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits")
      (let [sneakdoor (get-program state 0)]
        (card-ability state :runner sneakdoor 0)
        (is (= 1 (:credit (get-runner)))
            "Runner spends 1 additional credit to run with a card ability")
        (run-successful state)
        (run-on state :archives)
        (is (= 1 (:credit (get-runner)))
            "Runner doesn't spend 1 credit to make a run")
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit 1)
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (is (zero? (:credit (get-runner))) "Runner has 0 credits")
        (card-ability state :runner sneakdoor 0)
        (is (not (:run @state)) "No run was initiated")
        (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
        (is (zero? (:credit (get-runner))) "Runner has 0 credits"))))
  (testing "First run event each turn costs an additional credit"
    (do-game
      (new-game {:corp {:deck ["Service Outage"]}
                 :runner {:deck [(qty "Out of the Ashes" 2)]}})
      (play-from-hand state :corp "Service Outage")
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
      (play-from-hand state :runner "Out of the Ashes")
      (is (= 3 (:credit (get-runner)))
          "Runner spends 1 additional credit to run with a run event")
      (click-prompt state :runner "Archives")
      (run-successful state)
      (run-on state :archives)
      (is (= 3 (:credit (get-runner)))
          "Runner doesn't spend 1 credit to make a run")
      (run-successful state)
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "No") ; Out of the Ashes prompt
      (core/lose state :runner :credit 4)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Out of the Ashes")
      (is (empty? (get-in @state [:runner :prompt]))
          "Out of the Ashes was not played")
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")))
  (testing "Works when played on the runner's turn"
    (do-game
      (new-game {:corp {:id "New Angeles Sol: Your News"
                        :deck ["Service Outage"
                               "Breaking News"]}
                 :runner {:deck ["Hades Shard"]}})
      (trash-from-hand state :corp "Breaking News")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Hades Shard")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Service Outage" (:hand (get-corp))))
      (is (find-card "Service Outage" (:current (get-corp)))
          "Service Outage is in play")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (run-on state :archives)
      (is (zero? (:credit (get-runner)))
          "Runner spends 1 additional credit to make a run")))
(testing "Doesn't fire if already run when played on the runner's turn"
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck ["Service Outage"
                             "Breaking News"]}
               :runner {:deck ["Hades Shard"]}})
    (trash-from-hand state :corp "Breaking News")
    (take-credits state :corp)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :runner "No action")
    (core/gain state :runner :credit 3)
    (play-from-hand state :runner "Hades Shard")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Service Outage" (:hand (get-corp))))
    (is (find-card "Service Outage" (:current (get-corp)))
        "Service Outage is in play")
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (run-on state :archives)
    (is (= 1 (:credit (get-runner)))
        "Runner doesn't spend 1 additional credit to make a run")))
(testing "trashed and reinstalled on steal doesn't double remove penalty"
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck ["Service Outage"
                             "Breaking News"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Service Outage")
    (take-credits state :corp)
    (run-on state :remote1)
    (run-successful state)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Service Outage" (:discard (get-corp))))
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
    (run-on state :archives)
    (is (= 6 (:credit (get-runner))) "Runner spends 1 credit to make a run"))))
