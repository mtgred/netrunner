(ns game-test.cards.icebreakers.femme-fatale
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest femme-fatale
  ;; Femme Fatale counter test
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Femme Fatale" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 18)
    (let [iw (get-ice state :hq 0)]
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :runner (get-program state 0))
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :corp iw)
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))
