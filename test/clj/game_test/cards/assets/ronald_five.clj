(ns game-test.cards.assets.ronald-five
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ronald-five
  ;; Ronald Five - Runner loses a click every time they trash a Corp card
  (do-game
    (new-game {:corp {:deck ["Ronald Five" "Melange Mining Corp."]}})
    (play-from-hand state :corp "Ronald Five" "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 2 (:click (get-runner))) "Lost 1 click")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 3 [Credits] to trash")
    (is (zero? (:click (get-runner))) "Lost 1 click")))
