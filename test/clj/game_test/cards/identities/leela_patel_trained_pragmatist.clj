(ns game-test.cards.identities.leela-patel-trained-pragmatist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest leela-patel-trained-pragmatist
  ;; Leela Patel
  (testing "complicated interaction with mutiple Gang Sign"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"
                               "Hostile Takeover"
                               "Geothermal Fracking"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck [(qty "Gang Sign" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Geothermal Fracking" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Leela Patel: Trained Pragmatist")
      (click-card state :runner (get-content state :remote2 0))
      (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
      (click-prompt state :runner "Gang Sign")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
      (click-card state :runner (get-content state :remote3 0))
      (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
      (click-prompt state :runner "Done")))
  (testing "issues with lingering successful run prompt"
    (do-game
      (new-game {:corp {:id "NBN: Making News"
                        :deck ["Breaking News" "SanSan City Grid"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"}})
      (starting-hand state :corp ["SanSan City Grid"])
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :remote1 0))
      (is (not (:run @state)) "Run is over")))
  (testing "upgrades returned to hand in the middle of a run do not break the run. Issue #2008"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) "Shock!"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Sure Gamble"]}})
      (starting-hand state :corp ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (trash-from-hand state :corp "Project Atlas")
      (trash-from-hand state :corp "Shock!")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :hq 0))
      (is (not (get-content state :hq 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :rd 0))
      (is (not (get-content state :rd 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "Archives")
      (click-prompt state :runner "Shock!")
      (click-prompt state :runner "Project Atlas")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :archives 0))
      (is (not (get-content state :archives 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses"))))
