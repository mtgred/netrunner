(ns game-test.cards.upgrades.bernice-mai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bernice-mai
  ;; Bernice Mai
  (testing "Basic test - successful and unsuccessful"
    (do-game
      (new-game {:corp {:deck [(qty "Bernice Mai" 3) (qty "Hedge Fund" 3) (qty "Wall of Static" 3)]}})
      (starting-hand state :corp ["Bernice Mai" "Bernice Mai" "Bernice Mai"])
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Bernice Mai" "R&D")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (:tag (get-runner))))
      (is (= 2 (:credit (get-runner))) "Runner paid 3cr to trash Bernice")
      (core/rez state :corp (get-content state :remote2 0))
      (core/gain state :runner :credit 20)
      (run-empty-server state :remote2)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (not (get-content state :remote2 0)) "Bernice auto-trashed from unsuccessful trace")
      (is (not (:run @state)) "Run ended when Bernice was trashed from server")
      (core/rez state :corp (get-content state :rd 0))
      (run-empty-server state :rd)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (:card (first (:prompt (get-runner)))) "Accessing a card from R&D; not showing Bernice Mai as possible access")))
  (testing "interaction with Dedicated Response Team"
    (do-game
      (new-game {:corp {:deck [(qty "Bernice Mai" 3) "Dedicated Response Team"]}})
      (play-from-hand state :corp "Bernice Mai" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (:tag (get-runner))))
      (is (= 2 (:credit (get-runner))) "Runner paid 3cr to trash Bernice")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 1 meat damage"))))
