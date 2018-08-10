(ns game-test.cards.operations.exchange-of-information
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest exchange-of-information
  ;; Exchange of Information
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"]}})
      (score-agenda state :corp (find-card "Market Research" (:hand (get-corp))))
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner gained 2 tags")
      (take-credits state :corp)
      (is (zero? (:tag (get-runner))) "Runner lost 2 tags")
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 4 (:agenda-point (get-runner))))
      (is (= 3 (:agenda-point (get-corp))))
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 4 (:agenda-point (get-corp))))))
  (testing "Swapping a just scored Breaking News keeps the tags"
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"]}})
      (take-credits state :corp)
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (core/steal state :runner (find-card "Explode-a-palooza" (:hand (get-corp))))
      (take-credits state :runner)
      (score-agenda state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner gained 2 tags")
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 2 (:tag (get-runner))) "Still has tags after swap and before end of turn")
      (take-credits state :corp)
      (is (= 3 (:agenda-point (get-runner))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner does not lose tags at end of turn")))
  (testing "Swapping a 15 Minutes still keeps the ability. #1783"
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "15 Minutes"
                               "Project Beale"]}})
      (score-agenda state :corp (find-card "15 Minutes" (:hand (get-corp))))
      (take-credits state :corp)
      (core/gain state :runner :tag 1)
      (core/steal state :runner (find-card "Project Beale" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "15 Minutes" (:scored (get-corp))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 1 (:agenda-point (get-runner))))
      (is (zero? (count (:deck (get-corp)))))
      ;; shuffle back into R&D from runner's scored area
      (let [fifm (get-scored state :runner 0)]
        (card-ability state :corp fifm 0))
      (is (= 2 (:agenda-point (get-corp))))
      (is (zero? (:agenda-point (get-runner))))
      (is (= "15 Minutes" (:title (first (:deck (get-corp))))))
      (take-credits state :corp)
      (core/steal state :runner (find-card "15 Minutes" (:deck (get-corp))))
      (take-credits state :runner)
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 1 (:agenda-point (get-runner))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "15 Minutes" (:scored (get-runner))))
      (click-card state :corp (find-card "Project Beale" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      ;; shuffle back into R&D from corp's scored area
      (let [fifm (get-scored state :corp 0)]
        (card-ability state :corp fifm 0))
      (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))
  (testing "Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687"
    (do-game
      (new-game {:corp {:deck [(qty "Exchange of Information" 2) "Mandatory Upgrades"
                               "Global Food Initiative"]}})
      (score-agenda state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
      (take-credits state :corp)
      (core/gain state :runner :tag 1)
      (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (take-credits state :runner)
      (is (= 3 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 3 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Mandatory Upgrades" (:scored (get-runner))))
      (click-card state :corp (find-card "Global Food Initiative" (:scored (get-corp))))
      (is (= 2 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 3 (:click (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 4 (:click (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))))
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Global Food Initiative" (:scored (get-runner))))
      (click-card state :corp (find-card "Mandatory Upgrades" (:scored (get-corp))))
      (is (= 3 (:agenda-point (get-corp))))
      (is (= 2 (:agenda-point (get-runner))))
      (is (= 2 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 3 (:click (get-corp))))
      (is (= 3 (:click-per-turn (get-corp)))))))
