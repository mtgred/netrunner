(ns game-test.cards.operations.24-7-news-cycle
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "24-7-news-cycle"}
  twenty-four-seven-news-cycle
  ;; 24/7 News Cycle
  (testing "Breaking News interaction"
    (do-game
      (new-game {:corp {:deck [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-from-hand state :corp "Breaking News" "New remote")
      (let [ag1 (get-content state :remote1 0)
            ag2 (get-content state :remote2 0)]
        (score-agenda state :corp ag1)
        (score-agenda state :corp ag2)
        (take-credits state :corp)
        (is (zero? (:tag (get-runner)))) ; tags cleared
        (take-credits state :runner)
        (play-from-hand state :corp "24/7 News Cycle")
        (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Breaking News")
        (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
        (is (= 2 (:tag (get-runner))) "Runner given 2 tags")
        (take-credits state :corp 2)
        (is (= 2 (:tag (get-runner))) "Tags remained after Corp ended turn"))))
  (testing "Posted Bounty interaction -- Issue #1043"
    (do-game
      (new-game {:corp {:deck [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)]}})
      (play-from-hand state :corp "Posted Bounty" "New remote")
      (play-from-hand state :corp "Posted Bounty" "New remote")
      (let [ag1 (get-content state :remote1 0)
            ag2 (get-content state :remote2 0)]
        (score-agenda state :corp ag1)
        (click-prompt state :corp "No")
        (score-agenda state :corp ag2)
        (click-prompt state :corp "No")
        (play-from-hand state :corp "24/7 News Cycle")
        (click-card state :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Forfeited Posted Bounty")
        (click-card state :corp (find-card "Posted Bounty" (:scored (get-corp))))
        (click-prompt state :corp "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
        (is (= 1 (:tag (get-runner))) "Runner given 1 tag")
        (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
        (is (zero? (:agenda-point (get-corp))) "Forfeited Posted Bounty to 24/7 News Cycle"))))
  (testing "Swapped agendas are able to be used. #1555"
    (do-game
      (new-game {:corp {:deck ["24/7 News Cycle" "Chronos Project"
                               "Philotic Entanglement" "Profiteering"]}
                 :runner {:deck [(qty "Turntable" 3)]}})
      (score-agenda state :corp (find-card "Chronos Project" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Philotic Entanglement" (:hand (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (core/steal state :runner (find-card "Profiteering" (:hand (get-corp))))
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Philotic Entanglement" (:scored (get-corp))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (click-card state :corp (find-card "Chronos Project" (:scored (get-corp))))
      (is (= "Chronos Project" (:title (first (:rfg (get-corp))))))
      ;; shouldn't work on an agenda in the Runner's scored area
      (is (= 2 (count (:hand (get-runner)))))
      (click-card state :corp (find-card "Philotic Entanglement" (:scored (get-runner))))
      (is (= 2 (count (:hand (get-runner)))))
      ;; resolve 'when scored' ability on swapped Profiteering
      (is (= 8 (:credit (get-corp))))
      (click-card state :corp (find-card "Profiteering" (:scored (get-corp))))
      (click-prompt state :corp "3")
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 3 (:bad-publicity (get-corp))))
      (is (= 23 (:credit (get-corp))) "Gained 15 credits"))))
