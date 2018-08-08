(ns game-test.cards.events.interdiction
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest interdiction
  ;; Corp cannot rez non-ice cards during runner's turn
  (do-game
    (new-game {:corp {:deck ["Jeeves Model Bioroids" "Jackson Howard"]}
               :runner {:deck ["Street Peddler"
                               (qty "Interdiction" 3)]}})
    (starting-hand state :runner ["Street Peddler" "Interdiction"])
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :corp jeeves)
      (is (:rezzed (refresh jeeves)) "Jeeves is rezzed.  Interdiction not active when on Peddler")
      (play-from-hand state :runner "Interdiction")
      (core/rez state :corp jackson)
      (is (not (:rezzed (refresh jackson))) "Jackson is not rezzed"))))
