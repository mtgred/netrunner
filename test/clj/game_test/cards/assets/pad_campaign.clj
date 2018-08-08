(ns game-test.cards.assets.pad-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest pad-campaign
  ;; PAD Campaign
  (do-game
    (new-game {:corp {:deck ["PAD Campaign"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :corp pad)
      (take-credits state :corp)
      (let [credits (:credit (get-corp))]
        (take-credits state :runner)
        (is (= (+ 1 credits) (:credit (get-corp))) "Should gain 1 credit at start of turn from PAD Campaign")))))
