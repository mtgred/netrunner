(ns game-test.cards.operations.invasion-of-privacy
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Invasion of Privacy" 3)]}
               :runner {:deck [(qty "Sure Gamble" 2) "Fall Guy" (qty "Cache" 2)]}})
    (core/gain state :corp :click 3 :credit 6)
    ;; trash 2 cards
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "0") ; Runner won't match
    (is (= 5 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
      (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "0") ; Runner won't match
    (is (= 3 (count (:hand (get-runner)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (click-prompt state :corp (find-card "Fall Guy" (:hand (get-runner))))
      (is (empty? (get-in @state [:corp :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-runner)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :corp "Invasion of Privacy")
    (click-prompt state :corp "0") ; default trace
    (click-prompt state :runner "2") ; Runner matches
    (is (= 1 (:bad-publicity (get-corp))))))
