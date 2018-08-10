(ns game-test.cards.upgrades.jinja-city-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jinja-city-grid
  ;; Jinja City Grid - install drawn ice, lowering install cost by 4
  (testing "Single draws"
    (do-game
      (new-game {:corp {:deck ["Jinja City Grid" (qty "Vanilla" 3) (qty "Ice Wall" 3)]}})
      (starting-hand state :corp ["Jinja City Grid"])
      (core/gain state :corp :click 6)
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (dotimes [n 5]
        (core/click-draw state :corp 1)
        (click-prompt state :corp (-> (get-corp) :prompt first :choices first))
        (is (= 4 (:credit (get-corp))) "Not charged to install ice")
        (is (= (inc n) (count (get-in @state [:corp :servers :remote1 :ices]))) (str n " ICE protecting Remote1")))
      (core/click-draw state :corp 1)
      (click-prompt state :corp (-> (get-corp) :prompt first :choices first))
      (is (= 3 (:credit (get-corp))) "Charged to install ice")
      (is (= 6 (count (get-in @state [:corp :servers :remote1 :ices]))) "6 ICE protecting Remote1")))
  (testing "Drawing non-ice on runner's turn"
    (do-game
      (new-game {:corp {:deck ["Jinja City Grid" (qty "Hedge Fund" 3)]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (starting-hand state :corp ["Jinja City Grid"])
      (play-from-hand state :corp "Jinja City Grid" "HQ")
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Yes")
      (is (= :bogus (-> (get-corp) :prompt first :prompt-type)) "Corp has a bogus prompt to fake out the runner")
      (click-prompt state :corp "Carry on!")
      (click-prompt state :runner "No action"))))
