(ns game-test.cards.programs.datasucker
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered ICE
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Datasucker"]}})
      (play-from-hand state :corp "Fire Wall" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (play-from-hand state :runner "Datasucker")
      (let [ds (get-program state 0)
            fw (get-ice state :remote1 0)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh ds) :virus)))
        (run-empty-server state "Archives")
        (is (= 2 (get-counters (refresh ds) :virus)))
        (run-on state "Server 1")
        (run-continue state)
        (run-successful state)
        (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
        (run-on state "Server 1")
        (core/rez state :corp fw)
        (is (= 5 (:current-strength (refresh fw))))
        (card-ability state :runner ds 0)
        (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
        (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))
  (testing "does not affect next ice when current is trashed. Issue #1788"
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:deck ["Datasucker" "Parasite"]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (play-from-hand state :runner "Datasucker")
      (let [sucker (get-program state 0)
            spider (get-ice state :hq 0)
            wrap (get-ice state :hq 1)]
        (core/add-counter state :runner sucker :virus 2)
        (core/rez state :corp spider)
        (core/rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner "Spiderweb")
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh sucker) 0)
        (card-ability state :runner (refresh sucker) 0)
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Datasucker")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker")))))
