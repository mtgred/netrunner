(ns game-test.cards.events.careful-planning
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest careful-planning
  ;; Careful Planning - Prevent card in/protecting remote server from being rezzed this turn
  (do-game
    (new-game {:corp {:deck ["PAD Campaign" (qty "Vanilla" 2)]}
               :runner {:deck [(qty "Careful Planning" 2)]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "Server 1")
    (take-credits state :corp)
    (let [pad (get-content state :remote1 0)
          v1 (get-ice state :hq 0)
          v2 (get-ice state :remote1 0)]
      (play-from-hand state :runner "Careful Planning")
      (click-card state :runner v1)
      (is (:prompt (get-runner)) "Can't target card in central server")
      (click-card state :runner v2)
      (core/rez state :corp v2)
      (is (not (:rezzed (refresh v2))) "Prevented remote ICE from rezzing")
      (take-credits state :runner)
      (core/rez state :corp (refresh v2))
      (is (:rezzed (refresh v2)) "Rez prevention of ICE ended")
      (take-credits state :corp)
      (play-from-hand state :runner "Careful Planning")
      (click-card state :runner pad)
      (core/rez state :corp pad)
      (is (not (:rezzed (refresh pad))) "Prevented remote server contents from rezzing")
      (take-credits state :runner)
      (core/rez state :corp (refresh pad))
      (is (:rezzed (refresh pad)) "Rez prevention of asset ended"))))
