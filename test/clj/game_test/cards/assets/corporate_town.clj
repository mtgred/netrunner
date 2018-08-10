(ns game-test.cards.assets.corporate-town
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest corporate-town
  ;; Corporate Town
  (do-game
    (new-game {:corp {:deck ["Corporate Town" "Hostile Takeover"]}
               :runner {:deck ["Data Dealer"]}})
    (core/gain state :corp :click 1)
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Corporate Town" "New remote")
    (let [ct (get-content state :remote2 0)
          ht (get-scored state :corp 0)]
      (core/rez state :corp ct)
      (click-card state :corp ht)
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (take-credits state :runner)
      (card-ability state :corp ct 0)
      (click-card state :corp (get-resource state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Corporate Town should trash Data Dealer")
      (core/end-phase-12 state :corp nil)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Corporate Town shouldn't activate if there are no resources"))))
