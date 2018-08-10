(ns game-test.cards.operations.hangeki
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hangeki
  ;; Hangeki
  (doseq [choice ["Yes" "No"]]
    (testing (str "choosing to " (when (= choice "No") "not ") "access card")
      (do-game
        (new-game {:corp {:deck ["Hostile Takeover" "Dedicated Response Team" "Hangeki"]}})
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (play-from-hand state :corp "Dedicated Response Team" "New remote")
        (take-credits state :corp)
        (run-on state :remote2)
        (run-successful state)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (take-credits state :runner)
        (play-from-hand state :corp "Hangeki")
        (click-card state :corp (get-content state :remote1 0))
        (click-prompt state :runner choice)
        (if (= "Yes" choice)
          (do (click-prompt state :runner "Steal")
              (is (= 1 (:agenda-point (get-runner))) "Runner should steal Hostile Takeover")
              (is (= 1 (-> (get-corp) :rfg count)) "Hangeki should be removed from the game"))
          (do (is (empty? (:prompt (get-runner))) "Runner should have no more prompts as access ended")
              (is (= -1 (:agenda-point (get-runner))) "Runner should add Hangeki to their score area worth -1 agenda point")
              (is (zero? (-> (get-corp) :rfg count)) "Hangeki shouldn't be removed from the game")))))))
