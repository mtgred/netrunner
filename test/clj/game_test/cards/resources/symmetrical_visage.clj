(ns game-test.cards.resources.symmetrical-visage
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck [(qty "Symmetrical Visage" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (core/click-draw state :runner nil)
      (is (= 4 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :runner nil)
      (is (= 4 (:credit (get-runner))) "No credit gained from second click spent to draw")))
  (testing "Gain 1 credit the first and second time you click to draw each turn when GCS is installed"
    (do-game
      (new-game {:runner {:deck [(qty "Symmetrical Visage" 3)
                                 (qty "Gene Conditioning Shoppe" 3)
                                 "Fall Guy"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (= 1 (:credit (get-runner))))
      (core/click-draw state :runner nil)
      (is (= 2 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :runner nil)
      (is (= 3 (:credit (get-runner)))
          "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
      ;; Move Fall Guy back to deck
      (core/move state :runner (find-card "Fall Guy" (:hand (get-runner))) :deck)
      (core/click-draw state :runner nil)
      (is (= 3 (:credit (get-runner)))
          "No credit gained from third click spent to draw with Gene Conditioning Shoppe"))))
