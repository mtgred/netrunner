(ns game-test.cards.agendas.bacterial-programming
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bacterial-programming
  ;; Bacterial Programming
  (testing "Scoring should not cause a run to exist for runner."
    (do-game
      (new-game {:corp {:deck ["Bacterial Programming" "Hedge Fund"]}})
      (starting-hand state :corp ["Bacterial Programming"])
      (play-and-score state "Bacterial Programming")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Done")
      (click-prompt state :corp (first (:deck (get-corp))))
      (click-prompt state :corp "Done")
      (is (empty (:prompt (get-corp))) "Bacterial Programming prompts finished")
      (is (not (:run @state)) "No run is active")))
  (testing "Removing all cards from R&D should not freeze for runner, nor give an extra access."
    (do-game
      (new-game {:corp {:deck [(qty "Bacterial Programming" 8)]}}
                {:start-as :runner})
      (starting-hand state :corp [])
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      ;; Move all 7 cards to trash
      (doseq [_ (range 7)
              ;; Get the first card listed in the prompt choice
              ;; TODO make this function
              :let [card (-> @state
                             (get-in [:corp :prompt])
                             first
                             (get-in [:choices 0]))]]
        (click-prompt state :corp card))
      (click-prompt state :corp "Done")                          ; Finished with trashing
      (click-prompt state :corp "Done")                          ; Finished with move-to-hq (no cards to move)
      ;; Run and prompts should be over now
      (is (empty (:prompt (get-corp))) "Bacterial Programming prompts finished")
      (is (empty (:prompt (get-runner))) "Bacterial Programming prompts finished")
      (is (not (:run @state))))))
