(ns game-test.cards.operations.midseason-replacements
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Runner tags after they steal an agenda
  (do-game
    (new-game {:corp {:deck ["Midseason Replacements" "Breaking News"]}})
    (play-from-hand state :corp "Midseason Replacements")
    (is (= 3 (:click (get-corp))) "Midseason precondition not met; Corp not charged a click")
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [bn (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Stole Breaking News")
      (take-credits state :runner)
      (play-from-hand state :corp "Midseason Replacements")
      (click-prompt state :corp "0") ; default trace
      (click-prompt state :runner "0") ; Runner won't match
      (is (= 6 (:tag (get-runner))) "Runner took 6 tags"))))
