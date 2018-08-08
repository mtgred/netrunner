(ns game-test.cards.assets.team-sponsorship
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest team-sponsorship
  ;; Team Sponsorship
  (testing "Install from HQ"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand"))))
  (testing "Install from Archives"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (trash-from-hand state :corp "Adonis Campaign")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :corp tsp)
        (score-agenda state :corp ag1)
        (click-card state :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard"))))
  (testing "Multiple installs"
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"
                               (qty "Team Sponsorship" 2)
                               (qty "Adonis Campaign" 2)]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "New remote")
      (trash-from-hand state :corp "Adonis Campaign")
      (let [ag1 (get-content state :remote3 0)
            tsp2 (get-content state :remote2 0)
            tsp1 (get-content state :remote1 0)]
        (core/rez state :corp tsp1)
        (core/rez state :corp tsp2)
        (score-agenda state :corp ag1)
        (click-prompt state :corp "Team Sponsorship")
        (click-card state :corp (find-card "Adonis Campaign" (:discard (get-corp))))
        (click-prompt state :corp "New remote")
        (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
            "Adonis installed by Team Sponsorship")
        (is (= "Adonis Campaign" (:title (get-content state :remote5 0)))
            "Adonis installed by Team Sponsorship"))))
  (testing "Score 5 points in one window"
    (do-game
      (new-game {:corp {:deck [(qty "AstroScript Pilot Program" 3)
                               "Team Sponsorship"
                               "Breaking News"
                               "SanSan City Grid"]}})
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (core/gain state :corp :credit 100 :click 5)
      (core/rez state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "Server 1")
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (core/rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote1 1))
      (click-card state :corp (find-card "AstroScript Pilot Program" (:hand (get-corp))))
      (is (zero? (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript not resolved yet")
      (click-prompt state :corp "Server 1")
      (is (= 1 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript resolved")
      (card-ability state :corp (first (:scored (get-corp))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (card-ability state :corp (second (:scored (get-corp))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
      (click-prompt state :corp "Server 1")
      (card-ability state :corp (second (next (:scored (get-corp)))) 0)
      (click-card state :corp (get-content state :remote1 1))
      (core/score state :corp {:card (get-content state :remote1 1)})
      (click-prompt state :corp "Done")
      (is (= 7 (:agenda-point (get-corp))) "Scored 5 points in one turn"))))
