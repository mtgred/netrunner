(ns game-test.cards.upgrades.oberth-protocol
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest oberth-protocol
  ;; Oberth Protocol
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Oberth Protocol" "Oaktown Renovation"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Oberth Protocol" "Server 1")
    (play-from-hand state :corp "Oaktown Renovation" "Server 1")
    (take-credits state :corp)
    (take-credits state :runner)
    (let [oberth (get-content state :remote1 0)
          oak (get-content state :remote1 1) ]
      (core/rez state :corp (refresh oberth))
      (click-card state :corp (get-scored state :corp 0))
      (advance state oak)
      (is (= 2 (get-counters (refresh oak) :advancement)) "Oaktown should have 2 advancement tokens on it"))))
