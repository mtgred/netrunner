(ns game-test.cards.upgrades.tori-hanzo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Pup" "Tori Hanzō"]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Net Shield"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Pup" "HQ")
      (play-from-hand state :corp "Tori Hanzō" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Net Shield")
      (run-on state "HQ")
      (let [pup (get-ice state :hq 0)
            tori (get-content state :hq 0)
            nshld (get-program state 0)]
        (core/rez state :corp pup)
        (core/rez state :corp tori)
        (card-subroutine state :corp pup 0)
        (card-ability state :runner nshld 0)
        (click-prompt state :runner "Done")
        (is (empty? (:discard (get-runner))) "1 net damage prevented")
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Done") ; decline to prevent
        (is (= 1 (count (:discard (get-runner)))) "1 net damage; previous prevention stopped Tori ability")
        (run-jack-out state)
        (run-on state "HQ")
        (card-subroutine state :corp pup 0)
        (click-prompt state :runner "Done")
        (click-prompt state :corp "Yes")
        (is (= 2 (count (:discard (get-runner)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-runner)))))))
  (testing "with Hokusai Grid: Issue #2702"
    (do-game
      (new-game {:corp {:deck ["Tori Hanzō" "Hokusai Grid"]}})
      (core/gain state :corp :credit 5)
      (play-from-hand state :corp "Hokusai Grid" "Archives")
      (play-from-hand state :corp "Tori Hanzō" "Archives")
      (take-credits state :corp)
      (run-on state "Archives")
      (let [hg (get-content state :archives 0)
            tori (get-content state :archives 1)]
        (core/rez state :corp hg)
        (core/rez state :corp tori)
        (run-successful state)
        (click-prompt state :corp "No") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-runner)))) "1 net damage suffered")
        (click-prompt state :runner "Hokusai Grid")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Tori Hanzō")
        (click-prompt state :runner "No action")
        (is (and (empty (:prompt (get-runner))) (not (:run @state))) "No prompts, run ended")
        (run-empty-server state "Archives")
        (click-prompt state :corp "Yes") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 1 (:brain-damage (get-runner))) "1 brain damage suffered")
        (click-prompt state :runner "Hokusai Grid")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Tori Hanzō")
        (click-prompt state :runner "No action")
        (is (and (empty (:prompt (get-runner))) (not (:run @state))) "No prompts, run ended"))))
  (testing "breaking subsequent net damage: Issue #3176"
    (do-game
      (new-game {:corp {:deck ["Tori Hanzō" (qty "Pup" 2) (qty "Neural EMP" 2)]}})
      (core/gain state :corp :credit 8)
      (play-from-hand state :corp "Tori Hanzō" "New remote")
      (play-from-hand state :corp "Pup" "Server 1")
      (take-credits state :corp)
      (run-on state "Server 1")
      (let [tori (get-content state :remote1 0)
            pup (get-ice state :remote1 0)]
        (core/rez state :corp pup)
        (core/rez state :corp tori)
        (card-subroutine state :corp pup 0)
        (click-prompt state :corp "Yes") ; pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-runner)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-runner))))
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Neural EMP")
        (is (= 2 (count (:discard (get-runner)))) "Net damage processed correctly")))))
