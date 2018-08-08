(ns game-test.cards.events.cold-read
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, trash 1 program (cannot be prevented) used during this run.
  (do-game
    (new-game {:corp {:deck [(qty "Blacklist" 3)]}
               :runner {:deck ["Imp" (qty "Cold Read" 2)]}})
    (play-from-hand state :corp "Blacklist" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [bl (get-content state :remote1 0)]
      (play-from-hand state :runner "Cold Read")
      (click-prompt state :runner "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:runner :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state)
      (click-prompt state :runner "[Imp]: Trash card")
      (click-card state :runner (get-program state 0))
      (is (= 2 (count (:discard (get-runner)))) "Imp and Cold Read in discard")
      ; Cold Read works when Blacklist rezzed - #2378
      (core/rez state :corp bl)
      (play-from-hand state :runner "Cold Read")
      (click-prompt state :runner "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:runner :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state))))
