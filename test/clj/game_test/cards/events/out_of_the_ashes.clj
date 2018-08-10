(ns game-test.cards.events.out-of-the-ashes
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest out-of-the-ashes
  ;; Out of the Ashes - ensure card works when played/trashed/milled
  (do-game
    (new-game {:corp {:deck ["Kala Ghoda Real TV" "Underway Renovation"]}
               :runner {:deck [(qty "Out of the Ashes" 6)]}})
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Out of the Ashes")
    (click-prompt state :runner "Archives")
    (is (:run @state))
    (run-successful state)
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (is (zero? (count (:hand (get-runner)))))
    (is (= 5 (count (:discard (get-runner)))))
    (take-credits state :runner)
    (let [underway (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh underway)}))
    (is (= 6 (count (:discard (get-runner)))))
    (take-credits state :corp)
    ;; remove 5 Out of the Ashes from the game
    (dotimes [_ 5]
      (is (not (empty? (get-in @state [:runner :prompt]))))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Archives")
      (is (:run @state))
      (run-successful state))
    (click-prompt state :runner "No")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 5 (count (:rfg (get-runner)))))
    (take-credits state :runner)
    (take-credits state :corp)
    ;; ensure that if you decline the rfg, game will still ask the next turn
    (is (not (empty? (get-in @state [:runner :prompt]))))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Archives")
    (is (:run @state))
    (run-successful state)
    (is (zero? (count (:discard (get-runner)))))
    (is (= 6 (count (:rfg (get-runner)))))))
