(ns game-test.cards.resources.decoy
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest decoy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}
               :runner {:deck ["Decoy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Decoy")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count (:prompt (get-runner)))) "Runner prompted to avoid tag")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-runner)))) "Decoy trashed")
    (is (zero? (:tag (get-runner))) "Tag avoided")))
