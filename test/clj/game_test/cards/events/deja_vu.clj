(ns game-test.cards.events.deja-vu
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest deja-vu
  ;; Deja Vu - recur one non-virus or two virus cards
  (do-game
    (new-game {:runner {:deck [(qty "Déjà Vu" 2)
                               "Cache"
                               "Datasucker"
                               "Dirty Laundry"]}})
    (take-credits state :corp 3) ; pass to runner's turn
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Datasucker")
    (trash-from-hand state :runner "Dirty Laundry")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in hand prior to playing Déjà Vu")
    (play-from-hand state :runner "Déjà Vu")
    (click-prompt state :runner (find-card "Dirty Laundry" (:discard (get-runner))))
    (is (empty? (:prompt (get-runner))) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :runner "Déjà Vu")
    (click-prompt state :runner (find-card "Cache" (:discard (get-runner))))
    (is (not (empty? (:prompt (get-runner)))) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (click-prompt state :runner (find-card "Datasucker" (:discard (get-runner))))
    (is (= 3 (count (:hand (get-runner)))) "Three cards in after playing second Déjà Vu")))
