(ns game-test.cards.identities.the-foundry-refining-the-process
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-foundry-refining-the-process
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game {:corp {:id "The Foundry: Refining the Process"
                        :deck [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)]}})
      (starting-hand state :corp ["Accelerated Beta Test"])
      (play-from-hand state :corp "Accelerated Beta Test" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Eli 1.0" (:play-area (get-corp))))
      (click-prompt state :corp "Archives")
      (click-prompt state :corp "Yes")
      (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D")
      (is (= 1 (count (:hand (get-corp)))) "Added Eli 1.0 to HQ"))))
