(ns game-test.cards.resources.fan-site
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Corp scores an agenda
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Fan Site"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")))
  (testing "Don't trigger after swap with Exchange of Information. Issue #1824"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2) "Exchange of Information"]}
                 :runner {:deck ["Fan Site"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Fan Site" (:scored (get-runner))))
      (click-card state :corp (find-card "Hostile Takeover" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-runner))))
      (is (zero? (:agenda-point (get-corp))))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site swapped into Corp score area")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site not removed from Corp score area")))
  (testing "Runner can forfeit Fan Site"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Fan Site" "Data Dealer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (let [credits (:credit (get-runner))]
        (card-ability state :runner (get-resource state 0) 0)
        (click-card state :runner (get-scored state :runner 0))
        (is (zero? (count (:scored (get-runner)))) "Fan Site successfully forfeit to Data Dealer")
        (is (= (+ credits 9) (:credit (get-runner))) "Gained 9 credits from Data Dealer")))))
