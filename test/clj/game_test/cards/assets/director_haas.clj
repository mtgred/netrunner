(ns game-test.cards.assets.director-haas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest director-haas
  ;; Director Haas
  (do-game
    (new-game {:corp {:deck [(qty "Director Haas" 2)]}})
    (play-from-hand state :corp "Director Haas" "New remote")
    (play-from-hand state :corp "Director Haas" "Server 1")
    (click-prompt state :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Haas trashed")
    (is (zero? (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [dh (get-content state :remote1 0)]
      (core/rez state :corp dh))
    (is (= 2 (:click (get-corp))) "Corp should immediately gain a click")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 4 (:click (get-corp))) "Corp should have an extra click each turn")
    (take-credits state :corp)
    (take-credits state :runner 3)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should be back to 3 clicks")
    (is (= 1 (count (get-scored state :runner))) "Director Haas added to Runner score area")
    (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points")))
