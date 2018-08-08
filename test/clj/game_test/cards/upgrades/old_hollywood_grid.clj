(ns game-test.cards.upgrades.old-hollywood-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest old-hollywood-grid
  ;; Old Hollywood Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [ohg (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp ohg)
        (run-successful state)
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
        (click-card state :runner ohg)
        (click-prompt state :runner "No action")
        (core/steal state :runner (find-card "House of Knives" (:hand (get-corp))))
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Steal")
        (is (= 2 (count (:scored (get-runner)))) "2 stolen agendas"))))
  (testing "Central server"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (take-credits state :corp 2)
      (let [ohg (get-content state :hq 0)]
        (run-on state "HQ")
        (core/rez state :corp ohg)
        (run-successful state)
        ;; runner now chooses which to access.
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "No stolen agendas")
        (click-prompt state :runner "Old Hollywood Grid")
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))
  (testing "Gang Sign interaction. Prevent the steal outside of a run. #2169"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "Project Beale" 2)]}
                 :runner {:deck ["Gang Sign"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "HQ")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (core/rez state :corp (get-content state :hq 0))
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang sign fires
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "No stolen agendas")))
  (testing "Trash order"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" "Project Beale"]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (run-on state "Server 1")
        (core/rez state :corp ohg)
        (run-successful state)
        (is (empty? (:scored (get-runner))) "Start with no stolen agendas")
        ;; runner now chooses which to access.
        (click-card state :runner (refresh ohg))
        (click-prompt state :runner "Pay 4 [Credits] to trash") ;; trash OHG
        (click-card state :runner (refresh pb))
        (click-prompt state :runner "No action")
        (is (empty? (:scored (get-runner))) "End with no stolen agendas")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda"))))
  (testing "Steal other agendas"
    (do-game
      (new-game {:corp {:deck ["Old Hollywood Grid" (qty "Project Beale" 2)]}})
      (play-from-hand state :corp "Old Hollywood Grid" "New remote")
      (play-from-hand state :corp "Project Beale" "Server 1")
      (play-from-hand state :corp "Project Beale" "New remote")
      (take-credits state :corp)
      (let [ohg (get-content state :remote1 0)
            pb (get-content state :remote1 1)]
        (core/rez state :corp ohg)
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Steal")
        (is (= 1 (count (:scored (get-runner)))) "1 stolen agenda")))))
