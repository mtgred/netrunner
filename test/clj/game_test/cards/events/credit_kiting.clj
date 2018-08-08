(ns game-test.cards.events.credit-kiting
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest credit-kiting
  ;; Credit Kiting - After successful central run lower install cost by 8 and gain a tag
  (do-game
    (new-game {:corp {:deck ["PAD Campaign" "Ice Wall"]}
               :runner {:deck ["Credit Kiting" "Femme Fatale"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Credit Kiting")
    (is (= 3 (:click (get-runner))) "Card not played, successful run on central not made")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Credit Kiting")
    (click-card state :runner (find-card "Femme Fatale" (:hand (get-runner))))
    (is (= 4 (:credit (get-runner))) "Femme Fatale only cost 1 credit")
    (testing "Femme Fatale can still target ice when installed with Credit Kiting, issue #3715"
      (let [iw (get-ice state :rd 0)]
        (click-card state :runner iw)
        (is (:icon (refresh iw)) "Ice Wall has an icon")))
    (is (= 1 (:tag (get-runner))) "Runner gained a tag")))
