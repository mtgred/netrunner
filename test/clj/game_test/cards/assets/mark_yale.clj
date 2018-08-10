(ns game-test.cards.assets.mark-yale
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mark-yale
  ;; Mark Yale
  (do-game
    (new-game {:corp {:deck ["Mark Yale" "Project Atlas" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Mark Yale" "Project Atlas"])
    (core/gain state :corp :credit 100 :click 100)
    (play-from-hand state :corp "Mark Yale" "New remote")
    (play-from-hand state :corp "Project Atlas" "New remote")
    (let [mark (get-content state :remote1 0)
          atlas (get-content state :remote2 0)]
      (core/rez state :corp mark)
      (advance state atlas 5)
      (core/score state :corp {:card (refresh atlas)}))
    (let [mark (get-content state :remote1 0)
          scored-atlas (get-scored state :corp 0)
          credits (:credit (get-corp))]
      (card-ability state :corp mark 1)
      (click-card state :corp scored-atlas)
      (is (= (+ credits 3) (:credit (get-corp))) "Mark Yale spending an agenda counter should gain 3 credits")
      (card-ability state :corp scored-atlas 0)
      (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
      (is (= (+ credits 4) (:credit (get-corp))) "Spending an agenda counter for another reason should gain 1 credit")
      (card-ability state :corp mark 0)
      (is (= (+ credits 6) (:credit (get-corp))) "Mark Yale trashing itself should gain 2 credits"))))
