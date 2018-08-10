(ns game-test.cards.assets.rex-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rex-campaign
  ;; Rex Campaign
  (testing "Gain 5 credits"
    (do-game
      (new-game {:corp {:deck ["Rex Campaign"]}})
      (play-from-hand state :corp "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :corp rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (let [credits (:credit (get-corp))]
          (is (zero? (get-counters (refresh rex) :power)))
          (click-prompt state :corp "Gain 5 [Credits]")
          (is (= (+ 5 credits) (:credit (get-corp))))
          (is (= "Rex Campaign" (-> (get-corp) :discard first :title)))))))
  (testing "Lose 1 bad publicity"
    (do-game
      (new-game {:corp {:deck ["Rex Campaign"]}})
      (core/gain-bad-publicity state :corp 1)
      (play-from-hand state :corp "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :corp rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (zero? (get-counters (refresh rex) :power)))
        (click-prompt state :corp "Remove 1 bad publicity")
        (is (zero? (:bad-publicity (get-corp))) "Should not have the same amount of bad publicity")
        (is (= "Rex Campaign" (-> (get-corp) :discard first :title)))))))
