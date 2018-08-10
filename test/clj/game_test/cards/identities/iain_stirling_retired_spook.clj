(ns game-test.cards.identities.iain-stirling-retired-spook
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest iain-stirling-retired-spook
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game {:corp {:deck ["Breaking News"]}
               :runner {:id "Iain Stirling: Retired Spook"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))
