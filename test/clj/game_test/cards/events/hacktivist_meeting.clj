(ns game-test.cards.events.hacktivist-meeting
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hacktivist-meeting
  ;; Hacktivist Meeting
  ;; Trash a random card from corp hand while active
  ;; Make sure it is not active when hosted on Peddler
  (do-game
    (new-game {:corp {:deck [(qty "Jeeves Model Bioroids" 2)
                             (qty "Jackson Howard" 2)]}
               :runner {:deck ["Street Peddler"
                               (qty "Hacktivist Meeting" 3)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Hacktivist Meeting"])
    (play-from-hand state :runner "Street Peddler")
    (take-credits state :runner)
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :corp jeeves)
      (is (zero? (count (:discard (get-corp)))) "Nothing discarded to rez Jeeves - Hacktivist not active")
      (take-credits state :corp)
      (play-from-hand state :runner "Hacktivist Meeting")
      (core/rez state :corp jackson)
      (is (= 1 (count (:discard (get-corp)))) "Card discarded to rez Jackson - Hacktivist active"))))
