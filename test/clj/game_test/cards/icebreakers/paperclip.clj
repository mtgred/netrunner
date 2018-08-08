(ns game-test.cards.icebreakers.paperclip
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck [(qty "Paperclip" 2)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "Yes") ; install paperclip
      (run-continue state)
      (run-successful state)
      (is (not (:run @state)) "Run ended")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (is (empty? (:prompt (get-runner))) "No prompt to install second Paperclip")))
  (testing "firing on facedown ice shouldn't crash"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["Paperclip"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "0")))
  (testing "do not show a second install prompt if user said No to first, when multiple are in heap"
    (do-game
      (new-game {:corp {:deck [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Paperclip" 3)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 1))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (run-continue state)
      ;; we should get the prompt on a second ice even after denying the first.
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (core/jack-out state :runner)
      ;; Run again, make sure we get the prompt to install again.
      (run-on state "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip"))))
