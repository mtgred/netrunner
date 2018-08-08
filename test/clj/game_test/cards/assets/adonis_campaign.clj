(ns game-test.cards.assets.adonis-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adonis-campaign
  ;; Adonis Campaign
  (do-game
    (new-game {:corp {:deck ["Adonis Campaign"]}})
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (let [ac (get-content state :remote1 0)]
      (core/rez state :corp ac)
      (is (= 1 (:credit (get-corp))))
      (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            counters (get-counters (refresh ac) :credit)]
        (take-credits state :runner)
        (is (= (:credit (get-corp)) (+ credits 3)) "Gain 3 from Adonis")
        (is (= (get-counters (refresh ac) :credit) (- counters 3)) "9 counter remaining on Adonis")))))
