(ns game-test.cards.operations.preemptive-action
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 3)
                               "Preemptive Action"]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "forces you to take 3 if there are three, and removes itself from game"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 3)
                               (qty "Preemptive Action" 1)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (= 3 (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))))
  (testing "Shuffles all archives cards into R&D if Archives has less than 3 cards, and removes itself from game"
    (do-game
      (new-game {:corp {:deck [(qty "Subliminal Messaging" 2)
                               (qty "Preemptive Action" 1)]}})
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (zero? (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp))))))))
