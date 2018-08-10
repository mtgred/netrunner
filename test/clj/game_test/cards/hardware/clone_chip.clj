(ns game-test.cards.hardware.clone-chip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Datasucker" (qty "Clone Chip" 2)]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "Clone Chip")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner "Datasucker")
        (let [ds (get-program state 0)]
          (is (not (nil? ds)))
          (is (= (:title ds) "Datasucker"))))))
  (testing "don't show inavalid choices"
    (do-game
      (new-game {:runner {:deck ["Inti" "Magnum Opus" "Clone Chip"]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Inti")
      (trash-from-hand state :runner "Magnum Opus")
      (play-from-hand state :runner "Clone Chip")
      (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Magnum Opus" (:discard (get-runner))))
        (is (nil? (get-program state 0)) "No program was installed"))
      (let [chip (get-hardware state 0)]
        (is (not (nil? chip)) "Clone Chip is still installed")
        (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Inti" (:discard (get-runner))))
        (let [inti (get-program state 0)]
          (is (not (nil? inti)) "Program was installed")
          (is (= (:title inti) "Inti") "Program is Inti")
          (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left"))))))
