(ns game-test.cards.upgrades.product-placement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Runner accesses it
  (do-game
    (new-game {:corp {:deck ["Product Placement"]}})
    (play-from-hand state :corp "Product Placement" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [pp (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (is (= 9 (:credit (get-corp))) "Gained 2 credits from Runner accessing Product Placement")
      (click-prompt state :runner "Pay 2 [Credits] to trash") ; Runner trashes PP
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-corp)))
          "No credits gained when Product Placement accessed in Archives"))))
