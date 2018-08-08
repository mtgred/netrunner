(ns game-test.cards.assets.chairman-hiro
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Runner max hand size; add as 2 agenda points if Runner trashes him
  (do-game
    (new-game {:corp {:deck [(qty "Chairman Hiro" 2)]}})
    (play-from-hand state :corp "Chairman Hiro" "New remote")
    (play-from-hand state :corp "Chairman Hiro" "Server 1")
    (click-prompt state :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Hiro trashed")
    (is (zero? (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [hiro (get-content state :remote1 0)]
      (core/rez state :corp hiro)
      (is (= 3 (core/hand-size state :runner)) "Runner max hand size reduced by 2")
      (take-credits state :corp)
      (take-credits state :runner 3)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (= 2 (:credit (get-runner))) "Runner paid 6 credits to trash")
      (is (= 5 (core/hand-size state :runner)) "Runner max hand size restored to 5")
      (is (= 1 (count (get-scored state :runner)))
          "Chairman Hiro added to Runner score area")
      (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points"))))
