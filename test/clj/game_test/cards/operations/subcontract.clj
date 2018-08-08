(ns game-test.cards.operations.subcontract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest subcontract
  ;; Subcontract
  (testing "Don't allow second operation until damage prevention completes"
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 2) "Subcontract"]}
                 :runner {:deck ["Plascrete Carapace"]}})
      (take-credits state :corp)
      (core/gain state :runner :tag 1)
      (play-from-hand state :runner "Plascrete Carapace")
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Scorched Earth" (:hand (get-corp))))
      (is (and (= 1 (count (:prompt (get-corp)))) (= :waiting (-> (get-corp) :prompt first :prompt-type)))
          "Corp does not have Subcontract prompt until damage prevention completes")
      (click-prompt state :runner "Done")
      (is (not-empty (:prompt (get-corp))) "Corp can now play second Subcontract operation")))
  (testing "interaction with Terminal operations"
    (do-game
      (new-game {:corp {:deck [(qty "Hard-Hitting News" 2) "Subcontract"]}})
      (core/gain state :runner :tag 1)
      (take-credits state :corp)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (play-from-hand state :corp "Subcontract")
      (click-card state :corp (find-card "Hard-Hitting News" (:hand (get-corp))))
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 5 (:tag (get-runner))) "Runner has 5 tags")
      (is (empty? (:prompt (get-corp))) "Corp does not have a second Subcontract selection prompt"))))
