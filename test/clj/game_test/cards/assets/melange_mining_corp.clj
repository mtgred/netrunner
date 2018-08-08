(ns game-test.cards.assets.melange-mining-corp
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest melange-mining-corp
  ;; Melange Mining Corp.
  (do-game
    (new-game {:corp {:deck ["Melange Mining Corp."]}})
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (take-credits state :runner)
    (core/rez state :corp (get-content state :remote1 0))
    (let [mmc (get-content state :remote1 0)
          credits (:credit (get-corp))]
      (is (= 3 (:click (get-corp))) "Corp should have 3 clicks")
      (card-ability state :corp mmc 0)
      (is (zero? (:click (get-corp))) "Corp should have 0 clicks after using Melange Mining Corp ability")
      (is (= (+ credits 7) (:credit (get-corp))) "Corp should gain 7 credits from Melange Mining Corp ability"))))
