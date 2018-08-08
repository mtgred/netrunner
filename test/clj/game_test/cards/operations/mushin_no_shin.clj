(ns game-test.cards.operations.mushin-no-shin
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn
  (do-game
    (new-game {:corp {:deck [(qty "Mushin No Shin" 2) "Ronin" "Profiteering"]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (get-counters (refresh ronin) :advancement)) "3 advancements placed on Ronin")
      (core/rez state :corp (refresh ronin))
      (is (not (:rezzed (refresh ronin))) "Ronin did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh ronin))
      (is (:rezzed (refresh ronin)) "Ronin now rezzed")
      (play-from-hand state :corp "Mushin No Shin")
      (click-card state :corp (find-card "Profiteering" (:hand (get-corp))))
      (let [prof (get-content state :remote2 0)]
        (core/score state :corp (refresh prof))
        (is (empty? (:scored (get-corp))) "Profiteering not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh prof))
        (click-prompt state :corp "0")
        (is (= 1 (:agenda-point (get-corp))) "Profiteering was able to be scored")))))
