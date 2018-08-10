(ns game-test.cards.assets.jeeves-model-bioroids
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jeeves-model-bioroids
  ;; Jeeves Model Bioroids
  (do-game
    (new-game {:corp {:deck ["Jeeves Model Bioroids" "TGTBT"
                             (qty "Melange Mining Corp." 2)]}
               :runner {:deck [(qty "Ghost Runner" 3)]}})
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Ghost Runner")
    (play-from-hand state :runner "Ghost Runner")
    (take-credits state :runner)
    ; install 3 things
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click for credits
    (take-credits state :corp 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click to purge
    (core/do-purge state :corp 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;;click to advance
    (core/advance state :corp (get-content state :remote2 0))
    (core/advance state :corp (get-content state :remote2 0))
    (core/advance state :corp (get-content state :remote2 0))
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;; use 3 clicks on card ability - Melange
    (core/rez state :corp (get-content state :remote3 0))
    (card-ability state :corp (get-content state :remote3 0) 0)
    (is (= 1 (:click (get-corp))))
    (take-credits state :corp)
    (take-credits state :runner)
    ;; trash 3 resources
    (core/gain state :runner :tag 1)
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 2 (count (:discard (get-runner)))))
    (core/trash-resource state :corp nil)
    (click-card state :corp (get-resource state 0))
    (is (= 3 (count (:discard (get-runner)))))
    (is (= 1 (:click (get-corp))))))
