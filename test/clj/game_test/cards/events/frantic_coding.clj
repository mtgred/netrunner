(ns game-test.cards.events.frantic-coding
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest frantic-coding
  ;; Frantic Coding - Install 1 program, other 9 cards are trashed
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (starting-hand state :runner ["Frantic Coding"])
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 2 (:credit (get-runner))))
        (is (= 1 (count (:discard (get-runner)))))
        (click-prompt state :runner (find-card "Magnum Opus" (:deck (get-runner))))
        (is (= 1 (count (get-program state))))
        (is (= 2 (:credit (get-runner))) "Magnum Opus installed for free")
        (is (= 10 (count (:discard (get-runner))))))))
  (testing "Don't install anything, all 10 cards are trashed"
    (do-game
      (new-game {:runner {:deck ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (starting-hand state :runner ["Frantic Coding"])
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 1 (count (:discard (get-runner)))))
        (click-prompt state :runner "No install")
        (is (zero? (count (get-program state))))
        (is (= 11 (count (:discard (get-runner)))))))))
