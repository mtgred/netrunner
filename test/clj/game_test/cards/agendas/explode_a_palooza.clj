(ns game-test.cards.agendas.explode-a-palooza
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest explode-a-palooza
  ;; Explode-a-palooza
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Explode-a-palooza"]}})
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Steal")
      (is (= 12 (:credit (get-corp))) "Gained 5 credits")))
  (testing "Interaction with The Turning Wheel. Issue #1717."
    (do-game
      (new-game {:corp {:deck [(qty "Explode-a-palooza" 3)]}
                 :runner {:deck ["The Turning Wheel"]}})
      (starting-hand state :corp ["Explode-a-palooza" "Explode-a-palooza"])
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Steal")
      (let [ttw (get-resource state 0)]
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 1 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 12 (:credit (get-corp))) "Gained 5 credits")
        (run-empty-server state :rd)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "Steal")
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 2 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 17 (:credit (get-corp))) "Gained 5 credits")))))
