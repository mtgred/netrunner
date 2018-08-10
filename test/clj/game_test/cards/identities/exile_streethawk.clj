(ns game-test.cards.identities.exile-streethawk
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest exile-streethawk
  ;; Exile
  (testing "Simultaneous-resolution prompt shown for interaction with Customized Secretary"
    (do-game
      (new-game {:runner {:id "Exile: Streethawk"
                          :deck [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                 (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Customized Secretary" "Clone Chip"])
      (trash-from-hand state :runner "Customized Secretary")
      (play-from-hand state :runner "Clone Chip")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-card state :runner (find-card "Customized Secretary" (:discard (get-runner))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
      (click-prompt state :runner "Exile: Streethawk")
      (is (= 1 (count (:hand (get-runner)))) "Exile drew a card"))))
