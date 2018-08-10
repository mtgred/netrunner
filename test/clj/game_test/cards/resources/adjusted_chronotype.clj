(ns game-test.cards.resources.adjusted-chronotype
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adjusted-chronotype
  ;; Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Adjusted Chronotype" (qty "Beach Party" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 2 clicks and gained 1 click")))
  (testing "Chronotype to cancel out MCA click loss"
    (do-game
      (new-game {:corp {:deck ["MCA Austerity Policy"]}
                 :runner {:deck ["Adjusted Chronotype"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (take-credits state :runner)
      (play-from-hand state :corp "MCA Austerity Policy" "New remote")
      (let [mca (get-content state :remote1 0)]
        (core/rez state :corp mca)
        (card-ability state :corp mca 0)
        (is (= 1 (get-counters (refresh mca) :power)))
        (take-credits state :corp)
        ; runner does not lose a click
        (is (= 4 (:click (get-runner)))))))
  (testing "Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed"
    (do-game
      (new-game {:runner {:deck ["Adjusted Chronotype"
                                 (qty "Beach Party" 3)
                                 "Gene Conditioning Shoppe"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 2 clicks and gained 2 clicks")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 3 clicks and gained 2 clicks"))))
