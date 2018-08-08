(ns game-test.cards.assets.clyde-van-rite
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Runner not having credits/cards to trash
  (do-game
    (new-game {:corp {:deck ["Clyde Van Rite"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Restructure" 2) (qty "John Masanori" 2)]}})
    (play-from-hand state :corp "Clyde Van Rite" "New remote")
    (let [clyde (get-content state :remote1 0)]
      (core/rez state :corp clyde)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp in Step 1.2")
      ;; Runner has 1+ credit and chooses to pay 1 credit
      (card-ability state :corp clyde 0)
      (is (= 9 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (is (some #{"Pay 1 [Credits]" "Trash top card"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 8 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner can't pay 1 credit so must trash top card
      (core/lose state :runner :credit (:credit (get-runner)))
      (card-ability state :corp clyde 0)
      (is (zero? (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (is (some #{"Trash top card"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Trash top card")
      (is (zero? (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has 1+ card in Stack and chooses to trash 1 card
      (card-ability state :corp clyde 0)
      (is (= 4 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (is (some #{"Pay 1 [Credits]" "Trash top card"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Trash top card")
      (is (= 4 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      ;; Runner has no cards in Stack so must pay 1 credit
      (card-ability state :corp clyde 0)
      (is (= 8 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (is (some #{"Pay 1 [Credits]"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 7 (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (take-credits state :corp)
      (dotimes [_ 4]
        (core/click-credit state :runner nil))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      ;; Runner has no credits and no cards so nothing happens
      (card-ability state :corp clyde 0)
      (is (zero? (:credit (get-runner))))
      (is (zero? (count (:deck (get-runner)))))
      (is (empty? (-> @state :corp :prompt))))))
