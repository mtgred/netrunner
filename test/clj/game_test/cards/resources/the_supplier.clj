(ns game-test.cards.resources.the-supplier
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-supplier
  ;; The Supplier
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["The Supplier"
                                 "Plascrete Carapace"
                                 "Utopia Shard"
                                 "Hedge Fund"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (card-ability state :runner ts 0)
        (is (= 1 (count (-> @state :runner :prompt first :choices))))
        (click-card state :runner (find-card "Utopia Shard" (:hand (get-runner))))
        (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
        (take-credits state :runner)
        (take-credits state :corp)
        ;; Utopia Shard cannot be afforded and should not be in the prompt
        (click-card state :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (= 2 (:credit (get-runner)))
            "Runner charged 1 credit to install Plascrete off The Supplier")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))) "Runner ends turn with 5 credits")
        (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))
  (testing "Interaction with Kate discount. Issue #578."
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["The Supplier"
                                 "Plascrete Carapace"
                                 "Kati Jones"
                                 "Hedge Fund"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (core/lose state :runner :credit (:credit (get-runner)))
        (core/end-turn state :runner nil)
        (take-credits state :corp)
        (click-card state :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (zero? (:credit (get-runner))) "Kate discount applied")
        (is (= 1 (count (get-resource state))) "Plascrete installed"))))
  (testing "Brain chip mem is deducted when it is hosted and Supplier is trashed. Issue #2358"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
                 :runner {:deck ["The Supplier"
                                 "Brain Chip"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (is (= 4 (core/available-mu state)) "Runner has 4 MU")
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Brain Chip" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (take-credits state :runner)
        (core/gain state :runner :tag 1)
        (core/trash-resource state :corp nil)
        (click-card state :corp (get-resource state 0))
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU")))))
