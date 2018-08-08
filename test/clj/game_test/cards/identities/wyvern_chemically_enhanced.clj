(ns game-test.cards.identities.wyvern-chemically-enhanced
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 3)]}
               :runner {:id "Wyvern: Chemically Enhanced"
                        :deck [(qty "Sure Gamble" 2) "Corroder"
                               "Clone Chip" "Easy Mark"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Easy Mark")
    (play-from-hand state :runner "Corroder")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 2 [Credits] to trash")  ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Pay 2 [Credits] to trash")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner))))) "Sure Gamble still in Wyvern's discard")))
