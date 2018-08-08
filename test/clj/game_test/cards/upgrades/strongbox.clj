(ns game-test.cards.upgrades.strongbox
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest strongbox
  ;; Strongbox
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Strongbox" "House of Knives"]}})
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))) "Runner was not charged 1click")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner sb)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay [Click] to steal")
        (is (= 1 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Click cost even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Strongbox" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Strongbox" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [sb (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (run-empty-server state "Server 1")
        (click-card state :runner sb)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        (click-prompt state :runner "Pay [Click] to steal")
        (is (= 2 (:click (get-runner))) "Runner was charged 1click")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda")))))
