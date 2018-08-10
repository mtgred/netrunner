(ns game-test.cards.ice.sadaka
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sadaka
  ;; Sadaka
  (testing "Sub 1 - Look at the top 3 cards of R&D, arrange those or shuffle R&D. You may draw 1 card"
    (do-game
      (new-game {:corp {:deck ["Sadaka" (qty "Enigma" 3)]}})
      (starting-hand state :corp ["Sadaka"])
      (play-from-hand state :corp "Sadaka" "Archives")
      (let [sadaka (get-ice state :archives 0)]
        (take-credits state :corp)
        (run-on state "archives")
        (core/rez state :corp sadaka)
        (is (zero? (count (:hand (get-corp)))) "Corp starts with empty hand")
        (card-subroutine state :corp (refresh sadaka) 0)
        (click-prompt state :corp "Shuffle R&D")
        (click-prompt state :corp "Yes")
        (is (= 1 (count (:hand (get-corp)))) "Corp draws a card")
        (card-subroutine state :corp (refresh sadaka) 0)
        (click-prompt state :corp "Shuffle R&D")
        (click-prompt state :corp "No")
        (is (= 1 (count (:hand (get-corp)))) "Corp doesn't draw a card"))))
  (testing "Sub 2 - You may trash 1 card in HQ. If you do, trash 1 resource. Trash Sadaka."
    (do-game
      (new-game {:corp {:deck [(qty "Sadaka" 2) (qty "Enigma" 3)]}
                 :runner {:deck ["Bank Job"]}})
      (play-from-hand state :corp "Sadaka" "Archives")
      (play-from-hand state :corp "Sadaka" "HQ")
      (let [sadaka (get-ice state :archives 0)
            sadakaHQ (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Bank Job")
        (run-on state "archives")
        (core/rez state :corp sadaka)
        (is (= 3 (count (:hand (get-corp)))) "Corp starts with 3 cards in hand")
        (is (zero? (count (:discard (get-corp)))) "Corps starts with 0 cards in archives")
        (card-subroutine state :corp (refresh sadaka) 1)
        (click-prompt state :corp (find-card "Enigma" (:hand (get-corp))))
        (is (= 2 (count (:hand (get-corp)))) "Corp discards 1 card")
        (is (= 1 (count (:discard (get-corp)))) "1 card trashed")
        (click-prompt state :corp "Done")
        (is (= 2 (count (:discard (get-corp)))) "Sadaka trashed")
        (run-jack-out state)
        (run-on state "archives")
        (core/rez state :corp sadakaHQ)
        (is (= 2 (count (:hand (get-corp)))) "Corp starts with 2 cards in hand")
        (is (= 2 (count (:discard (get-corp)))) "Corps starts with 2 cards in archives")
        (is (zero? (count (:discard (get-runner)))) "Runner starts with 0 cards in discard")
        (card-subroutine state :corp (refresh sadakaHQ) 1)
        (click-prompt state :corp (find-card "Enigma" (:hand (get-corp))))
        (is (= 1 (count (:hand (get-corp)))) "Corp discards 1 card")
        (is (= 3 (count (:discard (get-corp)))) "1 card trashed")
        (click-card state :corp (get-resource state 0))
        (is (= 1 (count (:discard (get-runner)))) "Runner resource trashed")
        (is (= 4 (count (:discard (get-corp)))) "sadakaHQ trashed")))))
