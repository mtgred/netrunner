(ns game-test.cards.upgrades.valley-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest valley-grid
  ;; Valley Grid
  (testing "Reduce Runner max hand size and restore it even if trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Valley Grid" 3) (qty "Ice Wall" 3)]}})
      (play-from-hand state :corp "Valley Grid" "New remote")
      (take-credits state :corp 2)
      (run-on state "Server 1")
      (let [vg (get-content state :remote1 0)]
        (core/rez state :corp vg)
        (card-ability state :corp vg 0)
        (card-ability state :corp vg 0) ; only need the run to exist for test, just pretending the Runner has broken all subs on 2 ice
        (is (= 3 (core/hand-size state :runner)) "Runner max hand size reduced by 2")
        (is (= 2 (get-in (refresh vg) [:times-used])) "Saved number of times Valley Grid used")
        (run-successful state)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (take-credits state :runner 3)
        (is (= 5 (core/hand-size state :runner)) "Runner max hand size increased by 2 at start of Corp turn")))))
