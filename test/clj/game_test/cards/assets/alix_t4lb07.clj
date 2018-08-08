(ns game-test.cards.assets.alix-t4lb07
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest alix-t4lb07
  ;; Alix T4LB07
  (do-game
    (new-game {:corp {:deck ["Alix T4LB07" (qty "PAD Campaign" 3)]}})
    (play-from-hand state :corp "Alix T4LB07" "New remote")
    (let [alix (get-content state :remote1 0)]
      (core/rez state :corp alix)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (get-counters (refresh alix) :power)) "Two counters on Alix")
      (is (= 4 (:credit (get-corp))))
      (card-ability state :corp alix 0)
      (is (= 8 (:credit (get-corp))) "Gain 4 credits from Alix"))))
