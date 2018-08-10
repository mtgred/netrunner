(ns game-test.cards.assets.palana-agroplex
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest palana-agroplex
  ;; Pālanā Agroplex - Both players draw 1 at start of Corp turn
  (do-game
    (new-game {:corp {:deck ["Pālanā Agroplex" (qty "Hedge Fund" 3)]}})
    (starting-hand state :corp ["Pālanā Agroplex"])
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Pālanā Agroplex" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (core/end-phase-12 state :corp nil)
    (is (= 2 (count (:hand (get-corp)))) "Corp drew 1 from Agroplex")
    (is (= 2 (count (:hand (get-runner)))) "Runner drew 1 from Agroplex")))
