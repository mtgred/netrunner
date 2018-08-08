(ns game-test.cards.programs.crescentus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game {:corp {:deck ["Quandary"]}
               :runner {:deck ["Crescentus"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Crescentus")
    (run-on state "HQ")
    (let [cres (get-program state 0)
          q (get-ice state :hq 0)]
      (card-ability state :runner cres 0)
      (is (not (nil? (get-program state 0))) "Crescentus could not be used because the ICE is not rezzed")
      (core/rez state :corp q)
      (is (:rezzed (refresh q)) "Quandary is now rezzed")
      (card-ability state :runner cres 0)
      (is (nil? (get-program state 0)) "Crescentus could be used because the ICE is rezzed")
      (is (not (:rezzed (refresh q))) "Quandary is no longer rezzed"))))
