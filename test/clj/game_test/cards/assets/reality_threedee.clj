(ns game-test.cards.assets.reality-threedee
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Runner tagged)
  (do-game
    (new-game {:corp {:deck ["Reality Threedee"]}})
    (play-from-hand state :corp "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :corp r3d)
      (is (= 1 (:bad-publicity (get-corp))) "Took 1 bad pub on rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit")
      (take-credits state :corp)
      (core/gain state :runner :tag 1)
      (take-credits state :runner)
      (is (= 13 (:credit (get-corp))) "Gained 2 credits because Runner is tagged"))))
